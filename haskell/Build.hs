#!/usr/bin/env runhaskell

{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction #-}

import Development.Shake
import Development.Shake.FilePath
import SingleInclude(writeHeader, buildHeader)
import System.FilePath.Find (find, fileType, FileType(RegularFile, Directory), (==?))
import Control.Applicative ((<$>))
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import Data.Text (Text)
import Control.Monad (void)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

destDirPath, moduleSrcDirPath, enhedronDir, cppTestDir, testHarnessExeFile :: FilePath
destDirPath = "../build"
moduleSrcDirPath = "../modules"
enhedronDir = "Enhedron"
cppTestDir = "cpp/test/src"
testHarnessExeFile = "test-harness"

integrationTestExeFile, introductoryExampleFile :: FilePath
integrationTestExeFile = "integration-test"
introductoryExampleFile = "Introductory"

multiIncludeDestDir, singleIncludeDestDir, cmakeListsFlagsFile :: FilePath
multiIncludeDestDir = "multi-include"
singleIncludeDestDir = "single-include"
cmakeListsFile = "CMakeLists.txt"
cmakeListsFlagsFile = "CMakeLists.flags.txt"
licenseFile = "LICENSE_1.0.txt"

compilers = ["gcc", "clang-3.6"]
variants = ["Debug", "Release"]

moduleDestDirPath, singleHeaderDestFilePath, cmakeListsFlagsDestFilePath, licenseDestFilePath, pdfDestFilePath :: (?destName :: FilePath) => FilePath
moduleDestDirPath = destDirPath </> ?destName
singleHeaderDestFilePath = destDirPath </> ?destName  </> "cpp/single-include" </> ?destName <.> "h"
cmakeListsFlagsDestFilePath = moduleDestDirPath </> cmakeListsFlagsFile
licenseDestFilePath = moduleDestDirPath </> licenseFile
pdfDestFilePath = moduleDestDirPath </> ?destName <.> "pdf"

docExmaplesSrcDirPath :: (?sourceName :: FilePath) => FilePath
docExmaplesSrcDirPath = ".." </> cppTestDir </> ?sourceName </> "Examples"

docExamplesDestDirPath :: (?destName :: FilePath) => FilePath
docExamplesDestDirPath = moduleDestDirPath </> "docs/examples"

allModuleDestFilePaths :: (?destName :: FilePath) => [FilePath] -> [FilePath]
allModuleDestFilePaths allModuleSrcFilePaths = (moduleDestDirPath </>) <$> (dropDirectory 3 <$> allModuleSrcFilePaths)

dropDirectory :: Int -> FilePath -> FilePath
dropDirectory c
    | c <= 0 = id
    | otherwise = dropDirectory1 . dropDirectory (c - 1)

mkDir :: FilePath -> Action ()
mkDir dir = liftIO $ createDirectoryIfMissing True dir

copyHeader :: FilePath -> FilePath -> Action ()
copyHeader moduleDestDirPath input = do
    let dest = moduleDestDirPath </> (dropDirectory1 input)
    mkDir $ takeDirectory dest
    copyFile' input dest

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn = find (fileType ==? Directory) (fileType ==? RegularFile)

singleHeaderRule :: (?destName :: FilePath) => [FilePath] -> Text -> Rules ()
singleHeaderRule allDepSrcFilePaths contents = do
    singleHeaderDestFilePath %> \_ -> do
        need allDepSrcFilePaths
        liftIO $ writeHeader singleHeaderDestFilePath contents

targetHeader :: (?destName :: FilePath) => FilePath -> FilePath
targetHeader headerSrcFilePath = moduleDestDirPath </> (dropDirectory 1 headerSrcFilePath)

allSingleHeaderDepsRules :: (?destName :: FilePath) => [FilePath] -> Rules ()
allSingleHeaderDepsRules allDepSrcFilePaths =
    mapM_ singleHeaderDepsRule allDepSrcFilePaths
      where
        singleHeaderDepsRule srcHeader = targetHeader srcHeader %> \_ ->
            copyHeader moduleDestDirPath srcHeader

testLogTarget :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
testLogTarget compiler variant includeType name =
    destDirPath </> "test" </> compiler </> variant </> includeType </> name <.> "log"

testMatrix :: [FilePath] -> [FilePath] -> [(FilePath, FilePath)] -> [FilePath]
testMatrix compilers variants exeDetails =
    [testLogTarget c v t n | c <- compilers, v <- variants, (t, n) <- exeDetails ]

shortcutRules :: (?destName :: FilePath) => Rules ()
shortcutRules = do
    phony "docs" $ need [moduleDestDirPath </> ?destName <.> "pdf"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter destDirPath ["//*"]

    phony "quick" $ do
        testOutput <- readFile' $ testLogTarget "gcc" "Debug" multiIncludeDestDir testHarnessExeFile
        putNormal testOutput

defaultTargets :: (?destName :: FilePath, ?sourceName :: FilePath) => [FilePath] -> [FilePath] -> Rules ()
defaultTargets allModuleSrcFilePaths singleHeaderIncludes =
    let exesDetail = [(singleIncludeDestDir, introductoryExampleFile), (multiIncludeDestDir, testHarnessExeFile)]
        testLogFiles = testMatrix compilers variants exesDetail
    in
    want (
        [singleHeaderDestFilePath, pdfDestFilePath, licenseDestFilePath, cmakeListsFlagsDestFilePath] ++
        allModuleDestFilePaths allModuleSrcFilePaths ++
        testLogFiles ++
        (targetHeader <$> singleHeaderIncludes))

testRule :: (?destName :: FilePath, ?sourceName :: FilePath) => Rules ()
testRule = do
    destDirPath </> "test/*/*/*/*" <.> "log" %> \out -> do
        let tailPath = lastN 4 $ splitPath out
        let exePath = dropExtension (destDirPath </> (foldl (</>) "exe" tailPath))

        need [exePath]
        -- TODO: Output to test results file then generate test.ok file which is the real required target.
        -- Otherwise, we could fail the build, then run it again and succeed because the file is more recent.
        unit $ cmd (FileStdout out) exePath

docExamplesRule :: (?destName :: FilePath, ?sourceName :: FilePath) => Rules ()
docExamplesRule =
    docExamplesDestDirPath </> "*.cpp" %> \out -> do
        let srcFilePath = docExmaplesSrcDirPath </> takeFileName out

        need [srcFilePath]
        unit $ cmd (FileStdout out) "tail" "-n" "+8" srcFilePath

docRules :: (?destName :: FilePath, ?sourceName :: FilePath) => Rules ()
docRules = do
    let docsSrc = "../modules" </> ?sourceName </> "docs"
    let pdfDocBuildFile = destDirPath </> "docs/latex" </> ?destName <.> "pdf"
    pdfDocBuildFile %> \out -> do
        let docsDest = moduleDestDirPath </> "docs"
        putNormal "Building docs"
        allExamples <- getDirectoryFiles docExmaplesSrcDirPath ["//*"]
        allDocsSrcs <- getDirectoryFiles docsSrc ["//*"]
        need (((docsDest </> ) <$> allDocsSrcs) ++ ((docExamplesDestDirPath </>) <$> allExamples))
        unit $ cmd (Cwd docsDest) "make" "latexpdf" "html"

    pdfDestFilePath %> \out -> copyFile' pdfDocBuildFile pdfDestFilePath

syncRules :: (?destName :: FilePath, ?sourceName :: FilePath) => [FilePath] -> Rules ()
syncRules allModuleSrcFilePaths = do
    licenseDestFilePath %> \out -> copyFile' (".." </> licenseFile) out
    cmakeListsFlagsDestFilePath %> \out -> copyFile' (".." </> cmakeListsFlagsFile) out

    allModuleDestFilePaths allModuleSrcFilePaths &%> \_ -> do
        let excludes = ["cpp/", "docs/examples" ++ "/", licenseFile, cmakeListsFlagsFile, ?destName <.> "pdf"]
        let excludeFlags = foldr (\a b -> "--exclude" : a : b) [] (('/' :) <$> excludes)
        putNormal "Running rsync"
        need allModuleSrcFilePaths

        unit $ cmd "rsync" "-az" "--delete" excludeFlags ((moduleSrcDirPath </> ?sourceName) ++ "/") moduleDestDirPath

cmake :: (?destName :: FilePath) => [FilePath] -> FilePath -> FilePath -> Action ()
cmake additionalDependencies srcRoot exeDir = do
    absoluteSrcRoot <- liftIO $ canonicalizePath srcRoot

    need (cmakeListsFlagsDestFilePath : (srcRoot </> cmakeListsFile) : additionalDependencies)

    mkDir exeDir

    let [compiler, variant, _] = lastN 3 $ splitPath exeDir
    let (cxx, cc) = cxxCompiler compiler
    let env = [Cwd exeDir, AddEnv "CMAKE_BUILD_TYPE" variant, AddEnv "CXX" cxx, AddEnv "CC" cc]

    command_ env "cmake" [absoluteSrcRoot]
    command_ env "make" ["-j", "8"]
      where
        cxxCompiler "gcc/" = ("g++", "gcc")
        cxxCompiler "clang-3.6/" = ("clang++-3.6", "clang-3.6")
        cxxCompiler name = error ("Unknown C++ compiler " ++ name)

cppMultiIncludeExesRule :: (?destName :: FilePath, ?sourceName :: FilePath) => [FilePath] -> [FilePath] -> Rules ()
cppMultiIncludeExesRule allCppSrcFilePaths singleHeaderIncludes = do
    let multiIncludeDestDirPath = destDirPath </> "exe/*/*" </> multiIncludeDestDir
    let allMultiIncludeDestFiles = (multiIncludeDestDirPath </> ) <$> [testHarnessExeFile, integrationTestExeFile]

    allMultiIncludeDestFiles &%> \outs -> case outs of
        out : _ -> cmake (singleHeaderIncludes ++ allCppSrcFilePaths) ".." $ takeDirectory out
        _ -> return ()

cppSingleIncludeRules :: (?destName :: FilePath, ?sourceName :: FilePath) => [FilePath] -> [FilePath] -> Rules ()
cppSingleIncludeRules allCppSrcFilePaths singleHeaderIncludes = do
    let destCppTestDir = moduleDestDirPath </> cppTestDir
    let allCppDestFilePaths = (destCppTestDir </>) <$> (dropDirectory 5 <$> allCppSrcFilePaths)

    destDirPath </> "exe/*/*" </> singleIncludeDestDir </> introductoryExampleFile %> \out ->
        let additionalDeps = singleHeaderDestFilePath : allCppDestFilePaths in
        cmake additionalDeps moduleDestDirPath $ takeDirectory out

    allCppDestFilePaths &%> \_ -> do
        need allCppSrcFilePaths

        let srcCppTestDir = (".." </> cppTestDir </> ?sourceName) ++ "/"

        mkDir destCppTestDir
        unit $ cmd "rsync" "-az" "--delete" srcCppTestDir destCppTestDir

main :: IO ()
main = let { ?sourceName = "Test"; ?destName = "MosquitoNet" } in do
    allModuleSrcFilePaths <- allFilesIn moduleSrcDirPath
    allCppSourceFiles <- allFilesIn (".." </> cppTestDir </> ?sourceName)
    let inputHeader = enhedronDir </> ?sourceName <.> "h"
    (singleHeaderIncludes, singleHeaderContents) <- buildHeader inputHeader

    shakeArgs shakeOptions{shakeFiles = destDirPath} $ do
        defaultTargets allModuleSrcFilePaths singleHeaderIncludes
        shortcutRules
        syncRules allModuleSrcFilePaths
        docExamplesRule
        docRules
        testRule
        cppSingleIncludeRules allCppSourceFiles singleHeaderIncludes
        cppMultiIncludeExesRule allCppSourceFiles singleHeaderIncludes
        singleHeaderRule singleHeaderIncludes singleHeaderContents
        allSingleHeaderDepsRules singleHeaderIncludes
