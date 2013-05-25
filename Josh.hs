{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import System.Console.GetOpt
import System.Environment (getArgs)
import Data.List (foldl', isSuffixOf)
import qualified Data.ByteString.Char8 as B
import System.IO
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Aeson.Types (parseEither)
import Data.Either
import Data.Maybe
import System.Directory (doesFileExist, copyFile)
import Control.Monad.Trans.State
import qualified Data.Set as S
import Josh.Bootstrap (bootstrap)
import Josh.Visualizer (visualize, jsonValue)
import Control.Monad

data Flag = Output FilePath | LibPath FilePath | LibName String | Undefined String
                | Bootstrap FilePath | Visualize FilePath | Language String
                | IncludePath String deriving Show
data Config = Config { cOutput :: FilePath, cLibPaths :: [FilePath], cLibNames :: [String]
                    , cSpecial :: Maybe Flag } deriving Show
data SymInfo = SymInfo { siFile :: FilePath
                         , siOffset :: Integer
                         , siRegs :: [B.ByteString]
                         , siLength :: Int
                         , siCalls :: [B.ByteString] } deriving Show
type SymMap = M.Map B.ByteString SymInfo
type RegOffMap = (S.Set B.ByteString, [SymInfo])

options :: [OptDescr Flag]
options = [
            Option ['o'] [ "output" ] (ReqArg Output "OUTPUT") "Output into OUTPUT filename"
            , Option ['x'] [] (ReqArg Language "LANGUAGE") "Assume language of the input files"
            , Option ['I'] [] (ReqArg IncludePath "PATH") "Search for include files in PATH"
            , Option ['L'] [ "library-path" ] (ReqArg LibPath "PATH")
                "Search for libraries in PATH"
            , Option ['l'] [ "library" ] (ReqArg LibName "NAME") "Link with NAME library"
            , Option ['u'] [ "undefined" ] (ReqArg Undefined "SYMBOL")
                "Force SYMBOL as undefined"
            , Option [] [ "bootstrap" ] (ReqArg Bootstrap "CONFFILE")
                "Bootstrap GHC JavaScript installation"
            , Option [] [ "visualize" ] (ReqArg Visualize "LOGFILE")
                "Visualize trace file"
          ]

parseFlags :: [Flag] -> Config
parseFlags = foldl' go $ Config "a.out" [] [] Nothing where
    go c (Output f) = c { cOutput = f }
    go c (LibPath f) = c { cLibPaths = f:(cLibPaths c) }
    go c (LibName f) = c { cLibNames = f:(cLibNames c) }
    go c f@(Bootstrap _) = c { cSpecial = Just f }
    go c f@(Visualize _) = c { cSpecial = Just f }
    go c f@(Language _) = c { cSpecial = Just f }
    go c _ = c

markerBegin :: B.ByteString
markerBegin = "/* --josh-- ["

markerEnd :: B.ByteString
markerEnd = "] --josh-- */"

findInfos :: Handle -> IO ([Integer], [Integer])
findInfos h = go ([], []) where
    go res = do
        b <- hIsEOF h
        if b then return res
             else B.hGetLine h >>= more res >>= go
    more (bs, es) l | l == markerBegin = do o <- hTell h
                                            return ((o - 2):bs, es)
                    | l == markerEnd = do   o <- hTell h
                                            return (bs, (o - (toInteger $ B.length l)):es)
                    | otherwise = return (bs, es)

getBS :: Handle -> Integer -> Int -> IO B.ByteString
getBS h off len = hSeek h AbsoluteSeek off >> B.hGet h len

readInfoBytes :: Handle -> ([Integer], [Integer]) -> IO [B.ByteString]
readInfoBytes h = mapM go . uncurry zip where
    go (begin, end) = getBS h begin $ fromIntegral $ end - begin

parseSymbols :: FilePath -> Integer -> Value -> Either String SymMap
parseSymbols fp i = parseEither go where
    go (Array a) = do
        sis <- mapM syminfo $ V.toList a
        return $ M.fromList sis
    go j = fail $ "parseSymbols: bad root element: " ++ show j
    syminfo (Object o) = do
        sym <- o .: "name"
        off <- o .: "offset"
        len <- o .: "length"
        regs <- o .: "regs"
        calls <- o .: "calls"
        return (sym, SymInfo fp (i - off) regs len calls)
    syminfo j = fail $ "parseSymbols: bad function data: " ++ show j

loadSymbols :: FilePath -> IO [SymMap]
loadSymbols fp = withFile fp ReadMode go where
    go h = do
        is <- findInfos h
        bses <- readInfoBytes h is
        return $ rights $ map (uncurry (parseSymbols fp))
                        $ zip (map adj $ fst is) (map jsonValue bses)
    adj x = x - (toInteger $ B.length markerBegin) + 1

findLibrary :: [FilePath] -> String -> IO (Maybe FilePath)
findLibrary [] _ = return Nothing
findLibrary (x:xs) n = do
    b <- doesFileExist fn
    if b then return $ Just fn
         else findLibrary xs n
    where fn = x ++ "/lib" ++ n ++ ".a"

buildOffsetsMap :: [B.ByteString] -> State SymMap RegOffMap
buildOffsetsMap [] = return (S.empty, [])
buildOffsetsMap (x:xs) = do
    sm' <- get
    let (mv, sm) = M.updateLookupWithKey (\_ _ -> Nothing) x sm'
    p <- maybe (buildOffsetsMap []) ((put sm >>) . go) mv
    bu <- buildOffsetsMap xs
    return $ p `uni` bu
    where go si = do
            p <- buildOffsetsMap $ siCalls si
            return $ p `uni` (S.fromList $ siRegs si, [si])
          (rs1, offs1) `uni` (rs2, offs2) = (rs1 `S.union` rs2, offs1 ++ offs2)

initialStack :: [String]
initialStack = [ "stg_ap_v_info", "ZCMain_main_closure", "stg_enter_info" ]

writeSymInfo :: Handle -> Maybe (FilePath, Handle) -> SymInfo -> IO (Maybe (FilePath, Handle))
writeSymInfo h Nothing si = do
    rh <- openFile (siFile si) ReadMode
    getBS rh (siOffset si) (siLength si) >>= B.hPutStr h
    return $ Just ((siFile si), rh)
writeSymInfo h m@(Just (fp, rh)) si
    | siFile si == fp = do getBS rh (siOffset si) (siLength si) >>= B.hPutStr h
                           return m
    | otherwise = hClose rh >> writeSymInfo h Nothing si

type SymWrite = (Maybe (FilePath, Handle), S.Set (FilePath, Integer, Int))
writeSymInfoIfNeeded :: Handle -> SymWrite -> SymInfo -> IO SymWrite
writeSymInfoIfNeeded h t@(mb', set') si = if S.size set > S.size set'
    then do mb <- writeSymInfo h mb' si
            return (mb, set)
    else return t
    where set = S.insert (siFile si, siOffset si, siLength si) set'

writeOutput :: Maybe FilePath -> FilePath -> RegOffMap -> IO ()
writeOutput pjs fp (regs, offs) = withFile fp WriteMode go where
    go h = do
        maybe (return ()) (\p -> B.readFile p >>= B.hPutStr h) pjs

        B.hPutStr h "var "
        B.hPutStr h $ B.intercalate ", " $ S.toList $ S.delete "BaseReg" $ regs
        B.hPutStr h ";\n"
        
        foldM_ (writeSymInfoIfNeeded h) (Nothing, S.empty) offs
        hPutStr h $ "run_loop(createIOThread(null, null, ZCMain_main_closure));\n"

doLink :: Config -> [FilePath] -> IO ()
doLink cfg rest = do
    libs <- fmap catMaybes $ mapM (findLibrary $ cLibPaths cfg) $ cLibNames cfg
    syms <- mapM loadSymbols $ rest ++ libs
    pjs <- findLibrary (cLibPaths cfg) "ptr"
    writeOutput pjs (cOutput cfg) $ evalState (buildOffsetsMap $ map B.pack initialStack)
                                $ foldl' M.union M.empty $ concat syms

doLDScript :: FilePath -> FilePath -> IO ()
doLDScript ldf outf = do
    -- We expect ld script of the form: INPUT(x y)
    ld <- readFile ldf
    let os = words $ drop 1 $ takeWhile (/= ')') $ dropWhile (/= '(') ld
    withFile outf WriteMode (go os)
    where go :: [FilePath] -> Handle -> IO ()
          go os h = mapM_ (apnd h) os
          apnd h f = B.readFile f >>= B.hPutStr h

main :: IO ()
main = do
    (flags, rest, _) <- fmap (getOpt Permute options) getArgs
    let cfg = parseFlags flags
    maybe (pick_link cfg rest) (special cfg rest) $ cSpecial cfg
    where special _ _ (Bootstrap f) = bootstrap f
          special _ _ (Visualize f) = visualize f
          special cfg (x:_) (Language _) = copyFile x (cOutput cfg)
          special _ _ _ = return ()
          pick_link cfg (x:_) | ".ldscript" `isSuffixOf` x = doLDScript x (cOutput cfg)
          pick_link cfg rest = doLink cfg rest

