{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (forM_)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.ByteString.Search (replace)
import           System.Directory       (canonicalizePath, copyFile,
                                         doesFileExist, setCurrentDirectory)
import           System.Environment     (getArgs, setEnv)
import           System.FilePath        (replaceDirectory, splitDirectories,
                                         takeBaseName, takeDirectory, (</>))
import           System.IO.Temp         (withSystemTempDirectory)
import           System.Process         (readProcessWithExitCode)

data CheckerResult =
  CheckerFound
  | CheckerNotFound

processResult :: (t, t1, String) -> IO CheckerResult
processResult (code, stdout, stderr) = do
  let rlines = BS.lines (BS.pack stderr)
  forM_ rlines $ \line ->
    BS.putStrLn line
  return CheckerFound

compilerInvoke :: String -> String -> [BS.ByteString] -> IO CheckerResult
compilerInvoke tmp_file cdir stdparams = do
  let find_c x = if (not ("-" `BS.isPrefixOf` x)) && (".c" `BS.isSuffixOf` x) then BS.pack tmp_file else x
  let map_1 = map find_c stdparams
  let params =  map_1 ++ ["-fsyntax-only", "-fno-diagnostics-show-caret",
                          "-fno-diagnostics-color", "-iquote", BS.pack cdir]
  let str_params = BS.unwords params
  BS.putStrLn str_params
  setEnv "LANG" "en_US"
  (code, stdout, stderr) <- readProcessWithExitCode "bash" ["-c", BS.unpack str_params] ""
  processResult (code, stdout, stderr)

linuxChecker :: FilePath -> FilePath -> IO CheckerResult
linuxChecker tmp_file orig_src_file = do
  --
  -- Checker able to deal with source files of configured and
  -- built Linux kernel trees, or external kernel modules to
  -- those trees.
  --
  let dir = takeDirectory orig_src_file
  cdir <- canonicalizePath dir

  -- Find the .*.o.cmd file
  let base = takeBaseName orig_src_file
  let cmdfile = dir ++ "/." ++ base ++ ".o.cmd"
  e <- doesFileExist cmdfile
  if e then
    do
      -- Now we can know the compiler's command line, but we need to sanitize
      -- it a little.
      content <- BS.readFile cmdfile
      let flines = BS.lines content
      let (key, val) = BS.breakSubstring " := " (head flines)
      let split = BS.split ' ' $ BS.drop 4 val
      let filter_1 = filter (not . (BS.isPrefixOf "-Wp,")) split

      --
      -- Find the location of the kernel tree, as we must have it in order
      -- to build a kernel module.
      --
      -- For internal modules, the path is relative, so it is tiival.
      -- For external modules, there's more difficulty. On older trees
      -- there is an include to kconfig.h that can tell us, but on knewer
      -- kernels such information will only exist in the resulting .ko, which
      -- is too late in the build.
      --
      -- A slight modification to an external kernel module Makefile can
      -- pass a '-D__ORIG_KDIR__=' as a harmless addition to the command line,
      -- we can use it as a mini-protocol.
      --
      let kconfig = head $ filter (BS.isSuffixOf "/include/linux/kconfig.h") filter_1
      let orig_kdir_prefix = "-D__ORIG_KDIR__="
      let orig_kdir = head $ filter (orig_kdir_prefix `BS.isPrefixOf`) filter_1
      let kernel_dir =
            if | not ("cmd_/" `BS.isPrefixOf` key) -> -- Internal kernel tree code!
                   (iterate takeDirectory orig_src_file) !! (length $ splitDirectories $ BS.unpack $ BS.drop 5 key)
               | "/" `BS.isPrefixOf` kconfig ->
                   takeDirectory $ takeDirectory $ takeDirectory $ BS.unpack $ kconfig
               | otherwise ->
                   BS.unpack $ BS.drop (BS.length orig_kdir_prefix) orig_kdir

      setCurrentDirectory kernel_dir
      let fixslash x = BS.concat $ BL.toChunks $ replace "\\#" ("#" :: BL.ByteString) x
      compilerInvoke tmp_file cdir (map fixslash filter_1)
    else
      return CheckerNotFound

makefileChecker :: FilePath -> FilePath -> IO CheckerResult
makefileChecker tmp_file orig_src_file = do
  abs_src_file <- canonicalizePath orig_src_file
  check abs_src_file abs_src_file
  where
    check root dir = do
      e <- doesFileExist $ dir </> "Makefile"
      if e
        then do
           let
             params = [
               "check-syntax",
               "-C", dir, "CHK_SOURCES=" ++ tmp_file,
               "QUOTE_INCLUDE_DIRS=" ++ (show (takeDirectory root))]
           putStrLn $ "make " ++ unwords params
           (code, stdout, stderr) <- readProcessWithExitCode "make" params ""
           processResult (code, stdout, stderr)
        else do
           if dir == "/"
             then return CheckerNotFound
             else check root $ takeDirectory dir

standaloneChecker :: FilePath -> FilePath -> IO CheckerResult
standaloneChecker tmp_file orig_src_file = do
  let dir = takeDirectory orig_src_file
  cdir <- canonicalizePath dir
  if ".c" `BS.isSuffixOf` (BS.pack orig_src_file)
    then
    do let params = map BS.pack ["gcc", "-std=gnu11", "-O2", "-Wall",
                                 "-Wextra", "-o", "a.o", orig_src_file, "-c"]
       compilerInvoke tmp_file cdir params
    else
      return CheckerNotFound

mainIndirect :: FilePath -> FilePath -> IO ()
mainIndirect tmp_file orig_src_file =
  -- TODO: check for .c and .cc/.cpp extensions
  do let
       checkers = [
             linuxChecker
           , makefileChecker
           , standaloneChecker
         ]
     testCheckers checkers
  where testCheckers [] = return ()
        testCheckers (checker:others) = do
          r <- checker tmp_file orig_src_file
          case r of
            CheckerNotFound -> testCheckers others
            CheckerFound -> return ()

mainDirect :: FilePath -> IO ()
mainDirect src_file =
  withSystemTempDirectory "pacXXXXXX" $ \dir -> do
    let tmp_file = replaceDirectory src_file dir
    copyFile src_file tmp_file
    mainIndirect tmp_file src_file

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src_file] -> mainDirect src_file
    [tmp_file, orig_src_file] -> mainIndirect tmp_file orig_src_file
    _ -> putStrLn $ "Cannot work with argument " ++ (show args)
