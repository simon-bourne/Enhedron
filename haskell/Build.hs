#!/usr/bin/env runhaskell

{-# LANGUAGE ImplicitParams #-}

import Development.Shake
import Development.Shake.FilePath
import SingleInclude(writeHeader, buildHeader)
import System.FilePath.Find (find, fileType, FileType(RegularFile, Directory), (==?))
import Control.Applicative ((<$>))
import System.Directory (canonicalizePath)
import Data.Text (Text)
import Control.Monad (void)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

buildDir, moduleFilesDir, enhedron, cppTestDir, testHarnessExe, integrationTestExe, exampleExe :: FilePath
buildDir = "../build"
moduleFilesDir = "../modules"
enhedron = "Enhedron"
cppTestDir = "cpp/test/src"
testHarnessExe = "test-harness"
integrationTestExe = "integration-test"
exampleExe = "example"

multiIncludeDir, singleIncludeDir, cmakeListsFlagsFile :: FilePath
multiIncludeDir = "multi-include"
singleIncludeDir = "single-include"
cmakeListsFlagsFile = "CMakeLists.flags.txt"

compilers = ["gcc", "clang-3.6"]
variants = ["Debug", "Release"]

destDir, singleHeader :: (?destName :: FilePath) => FilePath
destDir = buildDir </> ?destName
singleHeader = buildDir </> ?destName  </> "cpp/single-include" </> ?destName <.> "h"

dropDirectory :: Int -> FilePath -> FilePath
dropDirectory c
    | c <= 0 = id
    | otherwise = dropDirectory1 . dropDirectory (c - 1)

mkDir :: FilePath -> Action ()
mkDir dir = unit $ cmd "mkdir" "-p" dir

copyHeader :: FilePath -> FilePath -> Action ()
copyHeader destDir input = do
    let dest = destDir </> (dropDirectory1 input)
    mkDir $ takeDirectory dest
    copyFile' input dest

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn = find (fileType ==? Directory) (fileType ==? RegularFile)

singleHeaderRules :: (?destName :: FilePath) => [FilePath] -> Text -> Rules ()
singleHeaderRules singleHeaderDeps singleHeaderContents = do
    let targetHeaders = singleHeader : ((destDir </>) <$> (dropDirectory 1 <$> singleHeaderDeps))

    want targetHeaders

    targetHeaders &%> \_ -> do
        mapM_ (copyHeader destDir) singleHeaderDeps
        liftIO $ writeHeader singleHeader singleHeaderContents

testLogTarget :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
testLogTarget compiler variant includeType name =
    buildDir </> "test" </> compiler </> variant </> includeType </> name <.> "log"

testMatrix :: [FilePath] -> [FilePath] -> [(FilePath, FilePath)] -> [FilePath]
testMatrix compilers variants exeDetails =
    [testLogTarget c v t n | c <- compilers, v <- variants, (t, n) <- exeDetails ]

shortcutRules :: (?destName :: FilePath) => Rules ()
shortcutRules = do
    phony "docs" $ need [destDir </> ?destName <.> "pdf"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter buildDir ["//*"]

    phony "quick" $ do
        testOutput <- readFile' $ testLogTarget "gcc" "Debug" multiIncludeDir testHarnessExe
        putNormal testOutput

rules :: (?destName :: FilePath, ?sourceName :: FilePath) => [FilePath] -> Rules ()
rules allModuleFiles = do
    let allModuleTargets = (destDir </>) <$> (dropDirectory 3 <$> allModuleFiles)
    let licenseFilename = "LICENSE_1.0.txt"
    let licenseTarget = destDir </> licenseFilename
    let pdfDocs = destDir </> ?destName <.> "pdf"
    let cmakeListsFlagsTarget = destDir </> cmakeListsFlagsFile

    let exesDetail = [(singleIncludeDir, exampleExe), (multiIncludeDir, testHarnessExe)]

    want ([pdfDocs, licenseTarget, cmakeListsFlagsTarget] ++ allModuleTargets ++ testMatrix compilers variants exesDetail)

    licenseTarget %> \out -> copyFile' (".." </> licenseFilename) out
    cmakeListsFlagsTarget %> \out -> copyFile' (".." </> cmakeListsFlagsFile) out

    buildDir </> "test/*/*/*/*" <.> "log" %> \out -> do
        let tailPath = lastN 4 $ splitPath out
        let exePath = dropExtension (buildDir </> (foldl (</>) "exe" tailPath))

        need [exePath]
        unit $ cmd (FileStdout out) exePath

    let examplesDir = "docs/examples"
    allModuleTargets &%> \_ -> do
        let excludes = ["cpp/", "docs/examples" ++ "/", licenseFilename, cmakeListsFlagsFile, ?destName <.> "pdf"]
        let excludeFlags = foldr (\a b -> "--exclude" : a : b) [] (('/' :) <$> excludes)
        putNormal "Running rsync"
        need allModuleFiles

        unit $ cmd "rsync" "-az" "--delete" excludeFlags ((moduleFilesDir </> ?sourceName) ++ "/") destDir

    -- TODO: Name all variables like path.
    let docsSrc = "../modules" </> ?sourceName </> "docs"
    let examplesDest = destDir </> examplesDir
    let examplesSrc = ".." </> cppTestDir </> ?sourceName </> "Examples"

    examplesDest </> "*.cpp" %> \out ->
        let filename = takeFileName out in
        unit $ cmd (FileStdout out) "tail" "-n" "+8" (examplesSrc </> filename)

    let pdfDocBuildFile = buildDir </> "docs/latex" </> ?destName <.> "pdf"
    pdfDocBuildFile %> \out -> do
        let docsDest = destDir </> "docs"
        putNormal "Building docs"
        allExamples <- getDirectoryFiles examplesSrc ["//*"]
        allDocsSrcs <- getDirectoryFiles docsSrc ["//*"]
        need (((docsDest </> ) <$> allDocsSrcs) ++ ((examplesDest </>) <$> allExamples))
        unit $ cmd (Cwd docsDest) "make" "latexpdf" "html"

    pdfDocs %> \out -> copyFile' pdfDocBuildFile pdfDocs

cmake :: (?destName :: FilePath) => [FilePath] -> FilePath -> FilePath -> Action ()
cmake additionalDependencies srcRoot exeDir = do
    let cmakeListsFile = "CMakeLists.txt"
    let cmakeListsFlagsTarget = destDir </> cmakeListsFlagsFile
    absoluteSrcRoot <- liftIO $ canonicalizePath srcRoot

    need (cmakeListsFlagsTarget : (srcRoot </> cmakeListsFile) : additionalDependencies)

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
    let multiIncludeTargetDir = buildDir </> "exe/*/*" </> multiIncludeDir
    let multiIncludeTargets = (multiIncludeTargetDir </> ) <$> [testHarnessExe, integrationTestExe]

    multiIncludeTargets &%> \outs -> case outs of
        out : _ -> cmake (singleHeaderIncludes ++ allCppSrcFiles) ".." $ takeDirectory out
        _ -> return ()

    let destCppTestDir = destDir </> cppTestDir
    let allCppSrcTargets = (destCppTestDir </>) <$> (dropDirectory 5 <$> allCppSrcFiles)

    buildDir </> "exe/*/*" </> singleIncludeDir </> exampleExe %> \out ->
        let additionalDeps = singleHeader : singleHeaderIncludes ++ allCppSrcTargets in
        cmake additionalDeps destDir $ takeDirectory out

    allCppSrcTargets &%> \_ -> do
        need allCppSrcFiles

        let srcCppTestDir = (".." </> cppTestDir </> ?sourceName) ++ "/"

        mkDir destCppTestDir
        unit $ cmd "rsync" "-az" "--delete" srcCppTestDir destCppTestDir

main :: IO ()
main = let { ?sourceName = "Test"; ?destName = "MosquitoNet" } in do
    allModuleFiles <- allFilesIn moduleFilesDir
    allCppSourceFiles <- allFilesIn (".." </> cppTestDir </> ?sourceName)
    let inputHeader = enhedron </> ?sourceName <.> "h"
    (singleHeaderIncludes, singleHeaderContents) <- buildHeader inputHeader

    shakeArgs shakeOptions{shakeFiles = buildDir} $ do
        shortcutRules
        rules allModuleFiles
        cppSrcRules allCppSourceFiles singleHeaderIncludes
        singleHeaderRules singleHeaderIncludes singleHeaderContents
