{-# LANGUAGE TupleSections #-}
module Main where
import Prompting.Display
import Prompting
import Parsing
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
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
                  T.writeFile f o
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
manipulate oc f = do
  (m,c) <- runMainUndoMenu mainMenu
  case m of
    Just ff -> T.writeFile (if ff == "" then f else ff) (serializeConfig $ c oc)
    Nothing -> return ()
  where
    mainMenu :: UndoDisplay (Maybe String, Config -> Config)
    mainMenu = do
      finalConfig <- mkUndoDisplay editConfig
      k <- boolUndoInput
      if k
        then do
        s <- stringUndoInput
        return (Just s, finalConfig)
        else return (Nothing, finalConfig)
        
    editConfig :: Display (Config -> Config)
    editConfig = do
      c <- charInput
      case c of
        't' -> withText "Edit piece types" editTypes
        'n' -> withText "Edit the map" editNodes
        '?' -> withText "Help" $ do
          displayHelp
          editConfig
        _ -> editConfig

    editTypes :: Display (Config -> Config)
    editTypes = do
      return id

    editNodes :: Display (Config -> Config)

    displayHelp :: Display ()
