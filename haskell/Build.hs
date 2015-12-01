#!/usr/bin/env runhaskell

import Development.Shake
import Development.Shake.FilePath
import SingleInclude(writeHeader, buildHeader)
import System.FilePath.Find (find, fileType, FileType(RegularFile, Directory), (==?))
import Control.Applicative ((<$>))
import System.Directory (canonicalizePath)
import Data.Text (Text)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

buildDir, moduleFilesDir, enhedron, cppTestDir :: FilePath
buildDir = "../build"
moduleFilesDir = "../modules"
enhedron = "Enhedron"
cppTestDir = "cpp/test/src"

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

singleHeaderRules :: FilePath -> [FilePath] -> Text -> Rules ()
singleHeaderRules destName singleHeaderDeps singleHeaderContents = do
    let singleHeader = singleHeaderName destName
    let targetHeaders = singleHeader : ((destDir </>) <$> (dropDirectory 1 <$> singleHeaderDeps))

    want targetHeaders

    targetHeaders &%> \_ -> do
        mapM_ (copyHeader destDir) singleHeaderDeps
        liftIO $ writeHeader singleHeader singleHeaderContents
      where
        destDir = buildDir </> destName

testLogTarget :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
testLogTarget compiler variant includeType name =
    buildDir </> "test" </> compiler </> variant </> includeType </> name <.> "log"

testMatrix :: [FilePath] -> [FilePath] -> [(FilePath, FilePath)] -> [FilePath]
testMatrix compilers variants exeDetails =
    [testLogTarget c v t n | c <- compilers, v <- variants, (t, n) <- exeDetails ]

rules :: FilePath -> FilePath -> [FilePath] -> [FilePath] -> Rules ()
rules sourceName destName allModuleFiles allCppSrcFiles = do
    let singleHeader = singleHeaderName destName
    let destCppTestDir = destDir </> cppTestDir
    let allCppSrcTargets = (destCppTestDir </>) <$> (dropDirectory 5 <$> allCppSrcFiles)
    let allModuleTargets = (destDir </>) <$> (dropDirectory 3 <$> allModuleFiles)
    let licenseFilename = "LICENSE_1.0.txt"
    let licenseTarget = destDir </> licenseFilename
    let htmlDocsTarget = "docs"
    let pdfDocs = destDir </> destName <.> "pdf"

    let testHarnessExe = "test-harness"
    let integrationTestExe = "integration-test"
    let exampleExe = "example"

    let multiIncludeDir = "multi-include"
    let singleIncludeDir = "single-include"

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "../build" ["//*"]

    phony "quick" $ do
        testOutput <- readFile' $ testLogTarget "gcc" "Debug" multiIncludeDir testHarnessExe
        putNormal testOutput

    let compilers = ["gcc", "clang-3.6"]
    let variants = ["Debug", "Release"]
    let exesDetail = [(singleIncludeDir, exampleExe), (multiIncludeDir, testHarnessExe)]

    want ([htmlDocsTarget, pdfDocs, licenseTarget] ++ allModuleTargets ++ testMatrix compilers variants exesDetail)

    licenseTarget %> \out -> copyFile' (".." </> licenseFilename) out

    buildDir </> "exe/*/*" </> singleIncludeDir </> exampleExe %> \out ->
        cmake (singleHeader : allCppSrcTargets) destDir $ takeDirectory out

    let multiIncludeTargetDir = buildDir </> "exe/*/*" </> multiIncludeDir
    let multiIncludeTargets = (multiIncludeTargetDir </> ) <$> [testHarnessExe, integrationTestExe]

    multiIncludeTargets &%> \outs -> case outs of
        out : _ -> cmake allCppSrcFiles ".." $ takeDirectory out
        _ -> return ()

    buildDir </> "test/*/*/*/*" <.> "log" %> \out -> do
        let tailPath = lastN 4 $ splitPath out
        let exePath = dropExtension (buildDir </> (foldl (</>) "exe" tailPath))

        need [exePath]
        unit $ cmd (FileStdout out) exePath

    allModuleTargets &%> \_ -> do
        putNormal "Running rsync"
        need allModuleFiles

        unit $ cmd "rsync" "-az" "--delete" "--exclude" "/cpp/" ((moduleFilesDir </> sourceName) ++ "/") destDir

    allCppSrcTargets &%> \_ -> do
        need allCppSrcFiles

        let srcCppTestDir = (".." </> cppTestDir </> sourceName) ++ "/"

        mkDir destCppTestDir
        unit $ cmd "rsync" "-az" "--delete" srcCppTestDir destCppTestDir

    let docsSrc = "../modules" </> sourceName </> "docs"
    pdfDocs %> \out -> do
        unit $ cmd (Cwd docsSrc) "make" "latexpdf"
        copyFile' (buildDir </> "docs/latex" </> destName <.> "pdf") pdfDocs

    phony htmlDocsTarget $ unit $ cmd (Cwd docsSrc) "make" "html"

      where
        destDir = buildDir </> destName

main :: IO ()
main = do
    allModuleFiles <- allFilesIn moduleFilesDir
    allCppSourceFiles <- allFilesIn (".." </> cppTestDir </> sourceName)
    let inputHeader = enhedron </> sourceName <.> "h"
    (singleHeaderIncludes, singleHeaderContents) <- buildHeader inputHeader

    shakeArgs shakeOptions{shakeFiles = buildDir} $ do
        rules sourceName destName allModuleFiles allCppSourceFiles
        singleHeaderRules destName singleHeaderIncludes singleHeaderContents

      where
        sourceName = "Test"
        destName = "MosquitoNet"
