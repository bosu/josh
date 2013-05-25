{-# OPTIONS -fno-warn-unused-do-bind #-}
module Main(main) where

import Test.Simple
import System.Process
import System.Exit (ExitCode(..))
import Control.Monad.IO.Class (liftIO)
import System.Directory
import Data.List
import Control.Monad
import System.Environment (getArgs, getEnv)
import Data.Maybe
import qualified Control.Exception as E

getEnvVar :: String -> IO (Either IOError String)
getEnvVar = E.try . getEnv

getProgs :: IO [String]
getProgs = do
    tfs <- getDirectoryContents "t/progs"
    return $ map (takeWhile (/= '.')) $ filter (isSuffixOf ".hs") tfs

jghc :: [String] -> TestSimpleT IO ExitCode
jghc ss = do
    diag cmd
    liftIO $ system $ cmd
    where cmd = concat ("./dist/js/bin/josh-ghc":ss)

testProg :: Bool -> String -> TestSimpleT IO ()
testProg with_opt fn = do
    plan 3

    liftIO $ createDirectory odir
    ec1 <- jghc [ " --make " ++ opt ++ " -outputdir ", odir, " -o "
                        , odir , "/o", " t/progs/", fn, ".hs" ]
    is ec1 ExitSuccess

    eitr <- liftIO $ getEnvVar "JOSH_TRACE"
    let trace = case eitr of
                        Left _ -> Nothing
                        Right s -> Just s

    more_js <- liftIO $ doesFileExist $ "t/progs/" ++ fn ++ ".js"
    when more_js $ do
        jsstr <- liftIO $ readFile $ "t/progs/" ++ fn ++ ".js"
        liftIO $ appendFile (odir ++ "/o") jsstr

    when (isJust trace) $ do
        void $ liftIO $ system $ "perl -pi -e 's#// trace#trace#;s#// capture#capture#' "
                                    ++ odir ++ "/o"
        void $ liftIO $ system $ "cat dist/js/rts/trace.js >> " ++ odir ++ "/o"

    ph <- liftIO $ runCommand $ "js " ++ odir ++ "/o 1> " ++ odir ++ "/out 2>" ++ odir ++ "/err"
    has_sh <- liftIO $ doesFileExist $ "t/progs/" ++ fn ++ ".sh"
    when has_sh $ liftIO $ void $ system $ "t/progs/" ++ fn ++ ".sh " ++ odir
    ec2 <- liftIO $ waitForProcess ph
    out <- liftIO $ readFile $ odir ++ "/out"
    err <- liftIO $ readFile $ odir ++ "/err"
    has_err <- liftIO $ doesFileExist $ "t/progs/" ++ fn ++ ".err"
    (esuc, eres) <- if has_err then do es <- liftIO $ readFile $ "t/progs/" ++ fn ++ ".err"
                                       return (ExitFailure 1, es)
                               else return (ExitSuccess, "")
    is ec2 esuc
    is err eres

    maybe (return ()) (\f -> liftIO $ writeFile f out) trace
    when (isNothing trace && more_js) $ do
        outstr <- liftIO $ readFile $ "t/progs/" ++ fn ++ ".out"
        plan 1
        is out outstr
        return ()

    return ()
    where odir = "t/progs/out/" ++ fn ++ opt
          opt = if with_opt then "-O2" else ""

main :: IO ()
main = do
    args <- getArgs
    b <- doesDirectoryExist "t/progs/out"
    when b $ removeDirectoryRecursive "t/progs/out"
    createDirectory "t/progs/out"

    prs <- fmap (filter blacklist) getProgs
    testSimpleMain $ do
        plan 2
        ec <- liftIO $ system $ concat [
            "cat dist/js/rts/libptr.a t/ptr_test.js | js"
            , " | diff -u t/ptr_test.out - > t/progs/out/js.diff" ]
        is ec ExitSuccess >>= guard

        rf2 <- liftIO $ readFile "t/progs/out/js.diff"
        is rf2 ""
        
        if length args > 0
             then testProg (length args /= 1) $ head args
             else do mapM_ (testProg False) prs
                     mapM_ (testProg True) prs

    where blacklist s = not $ elem s []
