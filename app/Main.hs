module Main where

import TokiPona.TokiPonaToEsperanto
import TokiPona.Version
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
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
       ,Option []       ["lang"]    (ReqArg Lang "xx")         "google tranlation in xx (iso-639-1), if supported."
       ,Option []       ["in"]      (ReqArg FileIn "FILE")     "input file with a tokipona text."
       ,Option []       ["out"]     (ReqArg FileOut "FILE")    "output file with the translation. (SOON)"
       ,Option ['h']    ["help"]    (NoArg Help)               "Print this help message"
       ,Option ['v']    []          (NoArg Version)            "Print the version of the tool"
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

        where header prgName = "Usage: "++ prgName ++ " [-dh] [--lang=fr|en|...] [--in=PATH] [--out=PATH] [word ...]\n\nCompound Word Dictionary size is "++ (show nbCompoundWord) ++"\n"



doTranslation useDico = if useDico then translateWithDico else TokiPona.TokiPonaToEsperanto.translate

getFlag :: (Flag -> Maybe String) -> [Flag] -> Maybe String
getFlag f [] = Nothing
getFlag f (flag:flags) = 
    case f flag of 
        Just s -> Just s 
        Nothing -> getFlag f flags

getLang = getFlag (\n -> case n of 
                        Lang s -> Just s 
                        _ -> Nothing )

getInFiles = getFlag (\n -> case n of 
                        FileIn s -> Just s 
                        _ -> Nothing )

getOutFiles = getFlag (\n -> case n of 
                        FileOut s -> Just s 
                        _ -> Nothing )  

source :: [Flag] -> [String] -> IO [String]
source args words = 
    case (getInFiles args, getOutFiles args) of 
        (Just pathIn, _) -> fmap lines (readFile pathIn) 
        (_, _) -> return ([unwords words])

main :: IO ()
main = do
    prgName <- getProgName
    (args, words) <- getArgs >>= parse prgName
    src <- source args words
    srcEO <- return (unlines $ map (doTranslation (Dictionary `elem` args)) src)
    mbk <- lookupEnv keyName
    putStrLn srcEO
    case (getLang args, mbk) of
        (Nothing, _) -> return ()
        (Just lang, Just key) -> do
            putStrLn $ "===> " ++ lang ++ " (translation with google) : "
            rep <- Google.Translate.translate key "eo" lang srcEO
            case rep of
                Right translation -> putStrLn translation
                Left error -> putStrLn $ "ERROR : " ++ error 
        (_, _) -> do
            putStrLn $ "no key " ++ keyName ++ "in your environement"
