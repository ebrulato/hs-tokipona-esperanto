module Main where

import TokiPona.TokiPonaToEsperanto
import TokiPona.Version
--import Control.Monad
--import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
--import Text.Printf
import Google.Translate

keyName = "API_KEY_GOOGLE_TRANSLATE"

data Flag
        = Dictionary            -- -d
        | Lang String           -- --lang=xx
        | FileIn String         -- --in=File
        | FileOut String        -- --out=File 
        | Help                  -- --help
        | Version               -- -v
        deriving (Show, Eq)


flags =
       [Option ['d']    []          (NoArg Dictionary)         "Use the conpound words tokipona dictionnary."
       ,Option []       ["lang"]    (ReqArg Lang "xx")         "google tranlation in xx (iso-639-1), if supported (SOON)"
       ,Option []       ["in"]      (ReqArg FileIn "FILE")     "input file with a tokipona text."
       ,Option []       ["out"]     (ReqArg FileOut "FILE")    "output file with the translation. (SOON)"
       ,Option ['h']    ["help"]    (NoArg Help)               "Print this help message"
       ,Option ['v']    []          (NoArg Version)            "Print the version of the tool"
       ]

parse prgName argv = case getOpt Permute flags argv of

        (args,w,[]) -> do
            let words = if null w then [] else w
            if Help `elem` args
            then do hPutStrLn stderr (usageInfo (header prgName) flags)
                    exitWith ExitSuccess
            else if Version `elem` args 
            then do hPutStrLn stdout (prgName ++ " " ++ TokiPona.Version.version)
                    exitWith ExitSuccess
            else return (nub args, words)

        (_,_,errs)      -> do
            hPutStrLn stderr (concat errs ++ usageInfo (header prgName) flags)
            exitWith (ExitFailure 1)

        where header prgName = "Usage: "++ prgName ++ " [-dh] [-lang=fr|en|...] [-in=PATH] [-out=PATH] [word ...]"



doTranslation useDico = if useDico then translateWithDico else TokiPona.TokiPonaToEsperanto.translate

getLang :: [Flag] -> Maybe String
getLang [] = Nothing
getLang (flag:flags) = 
    case flag of 
        Lang lang -> Just lang 
        _ -> getLang flags

getInFiles :: [Flag] -> Maybe String
getInFiles [] = Nothing
getInFiles (flag:flags) = 
    case flag of 
        FileIn path -> Just path 
        _ -> getInFiles flags

getOutFiles :: [Flag] -> Maybe String
getOutFiles [] = Nothing
getOutFiles (flag:flags) = 
    case flag of 
        FileOut path -> Just path 
        _ -> getOutFiles flags

source :: [Flag] -> [String] -> IO [String]
source args words = 
    case (getInFiles args, getOutFiles args) of 
        (Just pathIn, _) -> fmap lines (readFile pathIn) 
        (_, _) -> return ([unwords words])

main :: IO ()
main = do
    prgName <- getProgName
    (args, words) <- getArgs >>= parse prgName
    --putStrLn $ "Flags: " ++ show args
    --putStrLn $ "Words: " ++ show words
    src <- source args words
    srcEO <- return (unlines $ map (doTranslation (Dictionary `elem` args)) src)
    mbk <- lookupEnv keyName
    putStrLn srcEO
    case (getLang args, mbk) of
        (Nothing, _) -> return ()
        (Just lang, Just key) -> do
            putStrLn $ "===> " ++ lang ++ " (translation with google) : "
            rep <- Google.Translate.translate key "eo" lang srcEO
            putStrLn rep
        (_, _) -> do
            putStrLn $ "no key " ++ keyName ++ "in your environement"
