#!/usr/bin/env runhaskell

{-# LANGUAGE ImplicitParams #-}

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

-- TODO: All path generating funcs should be "description<Src|Dest><File|Dir|Path>
-- All rules are named "description<Rule>"

destDirPath, moduleSrcDirPath, enhedronDir, cppTestDir, testHarnessExeFile, integrationTestExeFile, introductoryExampleFile :: FilePath
destDirPath = "../build"
moduleSrcDirPath = "../modules"
enhedronDir = "Enhedron"
cppTestDir = "cpp/test/src"
testHarnessExeFile = "test-harness"
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

targetHeaders :: (?destName :: FilePath) => [FilePath] -> [FilePath]
targetHeaders allSingleHeaderSrcFilePaths =
    ((moduleDestDirPath </>) <$> (dropDirectory 1 <$> allSingleHeaderSrcFilePaths))

singleHeaderRules :: (?destName :: FilePath) => [FilePath] -> Text -> Rules ()
singleHeaderRules allSingleHeaderSrcFilePaths singleHeaderContents = do
    targetHeaders allSingleHeaderSrcFilePaths &%> \_ -> mapM_ (copyHeader moduleDestDirPath) allSingleHeaderSrcFilePaths
    singleHeaderDestFilePath %> \_ -> liftIO $ writeHeader singleHeaderDestFilePath singleHeaderContents

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

wantRules :: (?destName :: FilePath, ?sourceName :: FilePath) => [FilePath] -> [FilePath] -> Rules ()
wantRules allModuleSrcFilePaths singleHeaderIncludes =
    let exesDetail = [(singleIncludeDestDir, introductoryExampleFile), (multiIncludeDestDir, testHarnessExeFile)]
        testLogFiles = testMatrix compilers variants exesDetail
        individualFiles = [singleHeaderDestFilePath, pdfDestFilePath, licenseDestFilePath, cmakeListsFlagsDestFilePath]
    in
    want (individualFiles ++ allModuleDestFilePaths allModuleSrcFilePaths ++ testLogFiles ++ targetHeaders singleHeaderIncludes)

rules :: (?destName :: FilePath, ?sourceName :: FilePath) => [FilePath] -> Rules ()
rules allModuleSrcFilePaths = do
    licenseDestFilePath %> \out -> copyFile' (".." </> licenseFile) out
    cmakeListsFlagsDestFilePath %> \out -> copyFile' (".." </> cmakeListsFlagsFile) out

    destDirPath </> "test/*/*/*/*" <.> "log" %> \out -> do
        let tailPath = lastN 4 $ splitPath out
        let exePath = dropExtension (destDirPath </> (foldl (</>) "exe" tailPath))

        need [exePath]
        unit $ cmd (FileStdout out) exePath

    let examplesDir = "docs/examples"
    allModuleDestFilePaths allModuleSrcFilePaths &%> \_ -> do
        let excludes = ["cpp/", "docs/examples" ++ "/", licenseFile, cmakeListsFlagsFile, ?destName <.> "pdf"]
        let excludeFlags = foldr (\a b -> "--exclude" : a : b) [] (('/' :) <$> excludes)
        putNormal "Running rsync"
        need allModuleSrcFilePaths

        unit $ cmd "rsync" "-az" "--delete" excludeFlags ((moduleSrcDirPath </> ?sourceName) ++ "/") moduleDestDirPath

    -- TODO: Name all variables like path.
    let docsSrc = "../modules" </> ?sourceName </> "docs"
    let examplesDest = moduleDestDirPath </> examplesDir
    let examplesSrc = ".." </> cppTestDir </> ?sourceName </> "Examples"

    examplesDest </> "*.cpp" %> \out ->
        let filename = takeFileName out in
        unit $ cmd (FileStdout out) "tail" "-n" "+8" (examplesSrc </> filename)

    let pdfDocBuildFile = destDirPath </> "docs/latex" </> ?destName <.> "pdf"
    pdfDocBuildFile %> \out -> do
        let docsDest = moduleDestDirPath </> "docs"
        putNormal "Building docs"
        allExamples <- getDirectoryFiles examplesSrc ["//*"]
        allDocsSrcs <- getDirectoryFiles docsSrc ["//*"]
        need (((docsDest </> ) <$> allDocsSrcs) ++ ((examplesDest </>) <$> allExamples))
        unit $ cmd (Cwd docsDest) "make" "latexpdf" "html"

    pdfDestFilePath %> \out -> copyFile' pdfDocBuildFile pdfDestFilePath

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

cppSrcRules :: (?destName :: FilePath, ?sourceName :: FilePath) => [FilePath] -> [FilePath] -> Rules ()
cppSrcRules allCppSrcFiles singleHeaderIncludes = do
    let multiIncludeTargetDir = destDirPath </> "exe/*/*" </> multiIncludeDestDir
    let multiIncludeTargets = (multiIncludeTargetDir </> ) <$> [testHarnessExeFile, integrationTestExeFile]

    multiIncludeTargets &%> \outs -> case outs of
        out : _ -> cmake (singleHeaderIncludes ++ allCppSrcFiles) ".." $ takeDirectory out
        _ -> return ()

    let destCppTestDir = moduleDestDirPath </> cppTestDir
    let allCppSrcTargets = (destCppTestDir </>) <$> (dropDirectory 5 <$> allCppSrcFiles)

    destDirPath </> "exe/*/*" </> singleIncludeDestDir </> introductoryExampleFile %> \out ->
        let additionalDeps = singleHeaderDestFilePath : singleHeaderIncludes ++ allCppSrcTargets in
        cmake additionalDeps moduleDestDirPath $ takeDirectory out

    allCppSrcTargets &%> \_ -> do
        need allCppSrcFiles

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
        wantRules allModuleSrcFilePaths singleHeaderIncludes
        shortcutRules
        rules allModuleSrcFilePaths
        cppSrcRules allCppSourceFiles singleHeaderIncludes
        singleHeaderRules singleHeaderIncludes singleHeaderContents
