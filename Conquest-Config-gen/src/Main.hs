{-# LANGUAGE TupleSections #-}
module Main where
import Prompting
import Parsing
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Token hiding (lexeme)
import Text.Parsec.Combinator
import System.Directory
import System.Environment
import Control.Monad.State
import Data.Maybe
import Control.Applicative ((<$>), (<*>))
import qualified Control.Applicative as A
import Data.Char (chr, ord)

main :: IO ()
main = do
  args <- getArgs
  if args == []
    then prompt "Please input a file to edit" >>= main2
    else main2 $ head args
  where main2 f = do
          b <- doesFileExist f
          if b then do
            c <- parseConfigFile f
            case c of
              Left o -> do
                fcollaps <- promptYN "I found one or more includes. Should I collapse them into one file? (Warning: answering no will quit)"
                if fcollaps
                  then do
                  writeFile f o
                  main2 f
                  else
                  putStrLn "Unfortunately, I can't yet edit multi-file configs."
              Right c ->
                case c of
                  Left e -> do
                    putStrLn "I ran into a parsing error. Please fix your config and try again"
                    putStrLn "If you cannot find a mistake or don't know how to edit them, try starting from scratch."
                  Right c' -> manipulate c' f
            else manipulate (Cfg Map.empty []) f

manipulate :: Config -> String -> IO ()
manipulate c f = do
  promptOptions configOptions
  where
    runQuit :: Config -> String -> IO ()
    runQuit c f = do
      q <- promptYN "Do you want to save?"
      if q
        then do
        ff <- prompt $ "What file should I write to? (default:" ++ f ++ ")"
        writeFile (if ff == "" then f else ff) (serializeConfig c)
        else return ()
    configOptions :: Options
    configOptions = makeOptions
      [('q', "Quit", runQuit c f)]

