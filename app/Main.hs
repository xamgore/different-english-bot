module Main where

import Control.Monad
import Data.Maybe
import Data.List (find)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Lib


data Flag = Help | Version | Token String | Message String | ChatId String deriving (Show, Eq)

flags = [
        Option ['v'] [] (NoArg Version)
            "different english bot version 0.1.0",
        Option ['t'] [] (ReqArg Token "<token>")
            "token to connect with telegram api",
        Option ['m'] [] (ReqArg Message "<message>")
            "message that will be sent",
        Option ['c'] [] (ReqArg ChatId "<chatId>'")
            "chat id or @username",
        Option ['h'] [] (NoArg Help) ""
    ]

isToken (Token _)     = True
isToken _             = False

isMessage (Message _) = True
isMessage _           = False

isChat (ChatId _)     = True
isChat _              = False

parse :: [String] -> IO [Flag]
parse argv = case getOpt Permute flags argv of
       (args, _, []) -> return args
       (_, _, errs)  -> do
           hPutStrLn stderr (concat errs ++ usageInfo "" flags)
           exitFailure

version :: String
version = "dfbot version 0.1.0"

usage :: String
usage = "usage: dfbot [-v] [-h] [-c <chatId> -m <message> -t <token>]\n"


main :: IO ()
main = do
    args <- parse =<< getArgs

    when (Help `elem` args ||
          isNothing (find isChat args) ||
          isNothing (find isMessage args)) $
        hPutStrLn stderr (usageInfo usage flags) >> exitSuccess

    when (Version `elem` args) $
        putStrLn version >> exitSuccess

    let (Message msg) = fromJust (find isMessage args)
    let (ChatId chat) = fromJust (find isChat args)

    case find isToken args of
        Nothing ->
            putStr usage
        Just (Token tok) ->
            run tok $ sayToChannel chat msg
