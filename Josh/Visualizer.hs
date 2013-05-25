{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Josh.Visualizer (visualize, jsonValue) where

import qualified Data.IntMap as IM
import Data.List (groupBy, intercalate)
import System.Directory
import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Attoparsec (parse, IResult(Done))
import qualified Data.Text as T

data Location = Location { locFile :: FilePath, locLine :: Int } deriving Show
data Line = Line { liCaller :: Location, liSetters :: [Maybe Location]
                        , liIdx :: Int, liCreator :: Location }

makeLocation :: T.Text -> Location
makeLocation s = Location (js_file s) (ln_no s) where
    js_file = T.unpack . T.takeWhile (/= ':') . T.drop 1 . T.dropWhile (/= '(')
    ln_no = read . T.unpack . T.takeWhile (/= ':') . T.drop 1 . T.dropWhile (/= ':')

instance FromJSON Line where
    parseJSON (Object o) = do
        c <- o .: "caller"
        ss <- o .: "setters"
        i <- o .: "idx"
        cre <- o .: "creator"
        return $ Line (makeLocation c) (map mayloc ss) i $ makeLocation cre
        where mayloc (String te) = Just $ makeLocation te
              mayloc _ = Nothing

jsonValue :: B.ByteString -> Value
jsonValue bs = case parse json bs of
        (Done _ r) -> r
        e -> error $ "Bad parse: " ++ show e

parseLogFile :: FilePath -> IO [Line]
parseLogFile fn = do
    bs <- B.readFile fn
    case parseEither parseJSON $ jsonValue $ B.cons '[' $ B.snoc (B.drop 1 bs) ']' of
        Right res -> return res
        Left e -> error $ "parseLogFile: " ++ e

makeTable :: [(Int, Bool, String)] -> String
makeTable lns = unlines $ [
    "<html>"
    , "<head>"
    , "<script src=\"visualizer.js\"></script>"
    , "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">"
    , "</head>"
    , "<body>"
    , "<table>"
    ] ++ map row lns ++ [ 
    "</table>"
    , "</body>"
    , "</html>"
    ]
    where row (i, b, s) = "<tr><td><a href=\"#\" onclick=\"set_location(" ++ show i
                        ++ ");return false;\">" ++ show i ++ "</a></td><td "
                        ++ st b ++ "><a name=\"L"
                        ++ show i ++ "\">" ++ s ++ "</a></td></tr>"
          st b = if b then "class=\"selected\"" else ""

visualize :: FilePath -> IO ()
visualize fn = do
    ls <- fmap (map last . groupBy eqln) $ parseLogFile fn
    let lns = map liCaller ls
    let jsf = locFile $ head lns
    let lolis = map locLine lns
    let lm = IM.fromListWith (++) $ zip lolis $ map (\x -> [x]) [ 1::Int .. ]
    jslns <- fmap lines $ readFile jsf

    createDirectoryIfMissing True dir
    copyFile "etc/frameset.html" (dir ++ "/index.html")
    copyFile "etc/style.css" (dir ++ "/style.css")
    copyFile "etc/visualizer.js" (dir ++ "/visualizer.js")
    writeFile (dir ++ "/source.html") $ makeTable $ map (sour lm) (zip [1 .. ] jslns)
    writeFile (dir ++ "/milestones.html") $ unlines $ [
        "<html>"
        , "<head>"
        , "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">"
        , "<script src=\"visualizer.js\"></script>"
        , "</head>"
        , "<body>"
        ] ++ map spn lolis ++ [
        "</body>"
        , "</html>"
        ]
    writeFile (dir ++ "/arrays.html") $ makeTable $ map arra ls
    where sour lm (i, l) = (i, IM.member i lm, "<pre>" ++ l ++ "</pre>")
          anch l = "<a target=\"source\" href=\"source.html#L" ++ show l
                                ++ "\" onclick=\"set_location(" ++ show l ++ ")\">"
                                ++ show l ++ "</a>"
          spn l = "<span name=\"" ++ show l ++ "\">" ++ anch l ++ "</span>"
          dir = fn ++ "_report"
          arra l = (locLine $ liCaller l, False
                    , anch (locLine $ liCreator l) ++ ": " ++ (intercalate " "
                        $ map (idxwrap $ liIdx l)
                        $ zip [ 0 .. ] $ map sett $ liSetters l))
          eqln a b = (locLine $ liCaller a) == (locLine $ liCaller b)
          sett Nothing = "_"
          sett (Just loc) = anch $ locLine loc
          idxwrap i (n, s) | (i `div` 4) == n = "<span class=\"selected\">" ++ s ++ "</span>"
                           | otherwise = s
