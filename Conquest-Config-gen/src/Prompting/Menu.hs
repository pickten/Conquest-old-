{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prompting.Menu where
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Control.Monad.State

import Control.Applicative hiding ((<|>))
import Control.Arrow as Arrow
import Control.Monad.RWS
import Data.Char (toUpper)


data Menu s = Quit s | Menu s (Map Char (Menu s)) deriving (Eq, Functor)
type UndoMenu s = Menu (s, [Menu s])
type Switch s = Either s s

charInputMenu :: Menu Char
charInputMenu = Menu '\0' $ Map.fromList [(c, Quit c)| c<-['\0'..]]

awaitChar :: (Char -> Menu a) -> Menu a
awaitChar = await charInputMenu

stringInputMenu :: Menu String -- shuttup I know it's janky af
stringInputMenu = stringInputMenuFrom ""

stringInputMenuFrom :: String -> Menu String
stringInputMenuFrom s = Menu s $
      Map.fromList $ map (\c -> if c == '\n'
                       then ('\n', Quit s)
                       else (c, stringInputMenuFrom $ c:s)) $
      ['\0'..]

await :: Menu a -> (a -> Menu b) -> Menu b
await = (>>=)

awaitString :: (String -> Menu a) -> Menu a
awaitString = await stringInputMenu

getMenuState :: Menu s -> s
getMenuState x = case x of {Quit s->s; Menu s _ -> s}


addLastState :: Menu s -> Menu (s, Maybe (Menu s))
addLastState = lastState' Nothing
  where
    lastState' o (Quit s) = Quit (s, o)
    lastState' o x@(Menu s m) = Menu (s, o) $ fmap (lastState' $ Just x) m

addLastState' :: Menu s -> Menu (s, [Menu s])
addLastState' = innAddLastState' []

innAddLastState' :: [Menu s] -> Menu s -> UndoMenu s
innAddLastState' o (Quit s) = Quit (s, o)
innAddLastState' o x@(Menu s m) = Menu (s, o) $ fmap (innAddLastState' $ x:o) m

undo' :: Menu (s, Maybe (Menu s)) -> Menu s
undo' m = maybe (fmap fst m) id (snd $ getMenuState m)

undo :: UndoMenu s -> UndoMenu s
undo m = let
  m' = head (snd $ getMenuState m)
  m'' = tail (snd $ getMenuState m)
  in
  innAddLastState' m'' m'

bindUndoHere :: Menu s -> UndoMenu s
bindUndoHere x' = let
  x = addLastState' x'
  in fixify x
  where
    fixify a@(Quit s) = Quit s
    fixify a@(Menu (_,[]) _) = a
    fixify a@(Menu s m) = Menu s $
      Map.insertWith (flip const) '\\' ( awaitString $ (undoMany a) . (read :: String -> Int)) $
      Map.insertWith (flip const) '/' (undo a) m

bindUndo :: Menu s -> UndoMenu s
bindUndo (Quit s) = Quit (s,[])
bindUndo (Menu s m) = Menu (s,[]) $ fmap bindUndoHere m

undoMany :: UndoMenu s -> Int -> UndoMenu s
undoMany m i = innAddLastState' (drop i $ snd $ getMenuState m) ((snd $ getMenuState m) !!i)

getAncestry :: UndoMenu s -> [Menu s]
getAncestry = snd . getMenuState

instance Applicative Menu where
  pure = Quit
  (Quit f) <*> m = fmap f m
  (Menu f x) <*> (Quit s) = Quit $ f s
  (Menu f x) <*> (Menu s m) = Menu (f s) $ Map.intersectionWith (<*>) x m

instance Monad Menu where
  return  = pure
  (Quit s) >>= f = f s -- we want all branches from the leaves
  (Menu s m) >>= f = Menu (getMenuState $ f s) $ fmap (>>= f) m -- but we don't want intermediary ones


bindCharMenu :: Char -> Menu s -> Menu s -> Menu s
bindCharMenu c m (Quit s) = Quit s
bindCharMenu c m' (Menu s m) = Menu s $ Map.insert c m' m

trivialMenu :: s -> Menu s
trivialMenu s = Menu s $ Map.fromList [(c, Quit s)|c <-['\0'..]]
