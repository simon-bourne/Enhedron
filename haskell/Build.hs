#!/usr/bin/env runhaskell

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

multiIncludeDir, singleIncludeDir :: FilePath
multiIncludeDir = "multi-include"
singleIncludeDir = "single-include"


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

cmake :: [FilePath] -> FilePath -> FilePath -> Action ()
cmake additionalDependencies srcRoot exeDir = do
    let cmakeListsFile = "CMakeLists.txt"
    absoluteSrcRoot <- liftIO $ canonicalizePath srcRoot

    need ((srcRoot </> cmakeListsFile) : additionalDependencies)

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

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn = find (fileType ==? Directory) (fileType ==? RegularFile)

singleHeaderName :: FilePath -> FilePath
singleHeaderName destName = buildDir </> destName  </> "cpp/single-include" </> destName <.> "h"

singleHeaderRules :: FilePath -> FilePath -> [FilePath] -> Text -> Rules ()
singleHeaderRules destDir destName singleHeaderDeps singleHeaderContents = do
    let singleHeader = singleHeaderName destName
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

rules :: FilePath -> FilePath -> FilePath -> [FilePath] -> Rules ()
rules destDir sourceName destName allModuleFiles = do
    let singleHeader = singleHeaderName destName
    let allModuleTargets = (destDir </>) <$> (dropDirectory 3 <$> allModuleFiles)
    let licenseFilename = "LICENSE_1.0.txt"
    let licenseTarget = destDir </> licenseFilename
    let pdfDocs = destDir </> destName <.> "pdf"

    phony "docs" $ need [destDir </> destName <.> "pdf"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "../build" ["//*"]

    phony "quick" $ do
        testOutput <- readFile' $ testLogTarget "gcc" "Debug" multiIncludeDir testHarnessExe
        putNormal testOutput

    let compilers = ["gcc", "clang-3.6"]
    let variants = ["Debug", "Release"]
    let exesDetail = [(singleIncludeDir, exampleExe), (multiIncludeDir, testHarnessExe)]

    want ([pdfDocs, licenseTarget] ++ allModuleTargets ++ testMatrix compilers variants exesDetail)

    licenseTarget %> \out -> copyFile' (".." </> licenseFilename) out

    buildDir </> "test/*/*/*/*" <.> "log" %> \out -> do
        let tailPath = lastN 4 $ splitPath out
        let exePath = dropExtension (buildDir </> (foldl (</>) "exe" tailPath))

        need [exePath]
        unit $ cmd (FileStdout out) exePath

    allModuleTargets &%> \_ -> do
        let excludes = ["cpp/", licenseFilename, destName <.> "pdf"]
        let excludeFlags = foldr (\a b -> "--exclude" : a : b) [] (('/' :) <$> excludes)
        putNormal "Running rsync"
        need allModuleFiles

        unit $ cmd "rsync" "-az" "--delete" excludeFlags ((moduleFilesDir </> sourceName) ++ "/") destDir

    let docsSrc = "../modules" </> sourceName </> "docs"
    let pdfDocBuildFile = buildDir </> "docs/latex" </> destName <.> "pdf"
    pdfDocBuildFile %> \out -> do
        putNormal "Building PDF docs"
        getDirectoryFiles "" [docsSrc ++ "//*"] >>= need
        unit $ cmd (Cwd docsSrc) "make" "latexpdf" "html"

    pdfDocs %> \out -> copyFile' pdfDocBuildFile pdfDocs

cppSrcRules :: FilePath -> FilePath -> FilePath -> [FilePath] -> Rules ()
cppSrcRules destDir sourceName destName allCppSrcFiles = do
    let singleHeader = singleHeaderName destName
    let multiIncludeTargetDir = buildDir </> "exe/*/*" </> multiIncludeDir
    let multiIncludeTargets = (multiIncludeTargetDir </> ) <$> [testHarnessExe, integrationTestExe]

    multiIncludeTargets &%> \outs -> case outs of
        out : _ -> cmake allCppSrcFiles ".." $ takeDirectory out
        _ -> return ()

    let destCppTestDir = destDir </> cppTestDir
    let allCppSrcTargets = (destCppTestDir </>) <$> (dropDirectory 5 <$> allCppSrcFiles)

    buildDir </> "exe/*/*" </> singleIncludeDir </> exampleExe %> \out ->
        cmake (singleHeader : allCppSrcTargets) destDir $ takeDirectory out

    allCppSrcTargets &%> \_ -> do
        need allCppSrcFiles

        let srcCppTestDir = (".." </> cppTestDir </> sourceName) ++ "/"

        mkDir destCppTestDir
        unit $ cmd "rsync" "-az" "--delete" srcCppTestDir destCppTestDir

main :: IO ()
main = do
    allModuleFiles <- allFilesIn moduleFilesDir
    allCppSourceFiles <- allFilesIn (".." </> cppTestDir </> sourceName)
    let inputHeader = enhedron </> sourceName <.> "h"
    (singleHeaderIncludes, singleHeaderContents) <- buildHeader inputHeader

    shakeArgs shakeOptions{shakeFiles = buildDir} $ do
        let destDir = buildDir </> destName
        rules destDir sourceName destName allModuleFiles
        cppSrcRules destDir sourceName destName allCppSourceFiles
        singleHeaderRules destDir destName singleHeaderIncludes singleHeaderContents

      where
        sourceName = "Test"
        destName = "MosquitoNet"
