{-# LANGUAGE OverloadedStrings #-}
module Josh.Bootstrap (bootstrap) where

import Prelude hiding (lookup)
import Data.Configurator
import Data.Configurator.Types
import System.Directory
import System.Process
import System.Exit
import qualified Data.Text as T
import System.FilePath
import Control.Monad
import System.Environment.Executable (getExecutablePath)
import Data.List
import Data.Maybe
import System.Posix.Env (setEnv)

sh :: String -> IO ()
sh cmd = do
    ec <- system cmd
    if ec == ExitSuccess then return ()
                         else error $ "Failed: " ++ cmd

compileBaseLib :: FilePath -> FilePath -> FilePath -> Value -> IO ()
compileBaseLib base_dir jcab jreg (String lib') = do
    b <- doesDirectoryExist dir
    unless b $ sh $ "cd " ++ base_dir ++ " && git clone " ++ lib
    fls <- getDirectoryContents dir
    if isJust $ find (isSuffixOf ".conf") fls
        then return ()
        else compile $ elem "ghc-prim.cabal" fls
    where lib = drop_git_suffix $ T.unpack lib'
          dir = base_dir ++ "/" ++ takeFileName lib
          cd = "cd " ++ dir ++ " && "
          compile b = do when b $ sh $ cd ++ "cabal configure"
                         sh $ cd ++ jcab ++ " configure && " ++ jcab ++ " build"
                         sh $ cd ++ jreg
          drop_git_suffix l = if isSuffixOf ".git" l then reverse $ drop 4 $ reverse l else l

compileBaseLib _ _ _ e = error $ "Bad value: " ++ show e

writeSH :: FilePath -> String -> IO FilePath
writeSH fp ln = do
    writeFile fp $ unlines $ [ "#!/bin/sh", ln ]
    p <- getPermissions fp
    setPermissions fp (p { executable = True })
    canonicalizePath fp

cmmBuilds :: String -> String -> String -> [String] -> [String]
cmmBuilds ghc ghc_build lib_dir ss = (map go ss) ++ [cat] where
    go f = ghc ++ " -I" ++ ghc_build ++ "/rts -c -O2 "
                ++ ghc_build ++ "/rts/" ++ f ++ ".cmm -o " ++ o f
    cat = "cat " ++ (intercalate " " $ map o ss) ++ " > " ++ lib_dir ++ "/rts/libHSrts.a"
    o f = lib_dir ++ "/rts/" ++ (takeFileName f) ++ ".o"

bootstrap :: FilePath -> IO ()
bootstrap fp = do
    config <- load [ Required fp ]
    lib_dir' <- require config "lib_dir"
    createDirectoryIfMissing True $ lib_dir' ++ "/package.conf.d"
    lib_dir <- canonicalizePath lib_dir'
    let pkg_dir = lib_dir ++ "/package.conf.d"
    let bin_dir = lib_dir ++ "/bin"

    ghc_build <- require config "ghc_build_dir"
    let ghc = ghc_build ++ "/inplace/bin/ghc-stage2 -fjavascript"
    let ghc_pkg = ghc_build ++ "/inplace/lib/bin/ghc-pkg"

    createDirectoryIfMissing True $ lib_dir ++ "/rts"
    writeFile (lib_dir ++ "/rts/rts.conf") $ unlines [
            "name: rts"
            , "version: 1.0"
            , "id: builtin_rts"
            , "license: BSD3"
            , "exposed: True"
            , "library-dirs: " ++ lib_dir ++ "/rts"
            , "hs-libraries: HSrts"
            , "include-dirs: " ++ ghc_build ++ "/rts/dist/build"
            , "     " ++ ghc_build ++ "/includes"
            , "     " ++ ghc_build ++ "/includes/dist-derivedconstants/header"
        ]

    exe <- getExecutablePath
    createDirectoryIfMissing True bin_dir
    jghc <- writeSH (bin_dir ++ "/josh-ghc") $ concat [
                "GHC_PACKAGE_PATH=", pkg_dir
                , "\nexport GHC_PACKAGE_PATH\n"
                , "exec ", ghc, " -pgml ", exe, " -pgmc "
                , exe, " ${1+\"$@\"}" ]

    mapM_ sh $ cmmBuilds ghc ghc_build lib_dir
        [ "dist/build/AutoApply", "Apply", "HeapStackCheck", "StgMiscClosures"
                , "Updates", "PrimOps", "Exception", "StgStartup", "StgStdThunks" ]

    sh $ "gcc -traditional -P -E -I" ++ ghc_build ++ "/includes"
            ++ " -I" ++ ghc_build ++ "/includes/dist-derivedconstants/header -x c -o "
            ++ lib_dir ++ "/rts/libptr.a etc/ptr.js"
    sh $ "gcc -traditional -P -E -I" ++ ghc_build ++ "/includes"
            ++ " -I" ++ ghc_build ++ "/includes/dist-derivedconstants/header -x c -o "
            ++ lib_dir ++ "/rts/trace.js etc/trace.js"
    sh $ ghc_pkg ++ " --force --global-package-db"
                ++ " " ++ pkg_dir ++ " update " ++ lib_dir ++ "/rts/rts.conf"

    jpkg <- writeSH (bin_dir ++ "/josh-pkg") $ concat [
                "exec ", ghc_pkg, " --global-package-db=", pkg_dir, " ${1+\"$@\"}" ]

    jcab <- writeSH (bin_dir ++ "/josh-cabal") $ concat [
                "exec cabal --with-ghc=", jghc, " --with-ghc-pkg=", jpkg
                , " --with-hsc2hs=", ghc_build, "/inplace/bin/hsc2hs"
                , " --with-ld=", exe, " ${1+\"$@\"}" ]

    jreg <- writeSH (bin_dir ++ "/josh-register") $ unlines [
                "cabal register --gen-pkg-config --inplace"
                , jpkg ++ " register *.conf" ]

    setEnv "GHC_BUILD_DIR" ghc_build False

    let base_dir = lib_dir ++ "/libs"
    createDirectoryIfMissing True base_dir
    (List base_libs) <- require config "base_libs"
    mapM_ (compileBaseLib base_dir jcab jreg) base_libs
