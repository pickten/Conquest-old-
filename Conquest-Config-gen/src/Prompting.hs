{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prompting where
import Prompting.Menu
import Prompting.Display

{- VTY PROMPTS;
a significant chunk of this is cribbed from the example at
<https://github.com/coreyoconnor/vty/blob/master/Demo.hs>
-}

import Graphics.Vty (Vty, Event, Image, Picture, Key, Modifier, (<->), (<|>))
import qualified Graphics.Vty as V
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Control.Monad.State

import Control.Applicative hiding ((<|>))
import Control.Arrow as Arrow
import Control.Monad.RWS
import Data.Char (toUpper)

import Data.Default (def)
import Data.Foldable

eventToChar :: Event -> Maybe Char
eventToChar (V.EvKey k xs) = keyToChar k xs
  where
    keyToChar :: Key -> [Modifier] -> Maybe Char
    keyToChar k [] = unModKTC k
    keyToChar k [V.MShift] = capsKTC k
    keyToChar _ _ = Nothing -- no modifiers for you!

    unModKTC :: Key -> Maybe Char
    unModKTC k = case k of
      V.KChar c -> Just c
      V.KBS -> Just ' '
      V.KEnter -> Just '\n'
      _ -> Nothing
      
      
    capsKTC :: Key -> Maybe Char
    capsKTC = fmap toUpper . unModKTC
    
eventToChar _ = Nothing
  

eventBufferSize = 1000

type Prompter s = RWST Vty () (Display s) IO
type UndoPrompter s = RWST Vty () (UndoDisplay s) IO

runMainMenu :: Display s -> IO s
runMainMenu m = do
  vty <- if True -- change to False for emacs-like input processing
            then V.mkVty def
            else V.mkVty (def { V.vmin = Just 2, V.vtime = Just 300 } )
  (Display (x, _, _), _) <- execRWST (vtyInteract False) vty m
  V.shutdown vty
  return $ getMenuState x


runMainUndoMenu :: UndoDisplay s -> IO s
runMainUndoMenu m = do
  vty <- if True -- change to False for emacs-like input processing
            then V.mkVty def
            else V.mkVty (def { V.vmin = Just 2, V.vtime = Just 300 } )
  (UndoDisplay (Display (x, _, _)), _) <- execRWST (vtyUndoInteract False) vty m
  V.shutdown vty
  return $ fst $ getMenuState x


vtyInteract :: Bool -> Prompter s ()
vtyInteract shouldExit = do
    updateDisplay
    unless shouldExit $ handleNextEvent >>= vtyInteract



vtyUndoInteract :: Bool -> UndoPrompter s ()
vtyUndoInteract shouldExit = do
    updateUndoDisplay
    unless shouldExit $ handleUndoNextEvent >>= vtyUndoInteract


updateDisplay :: Prompter s ()
updateDisplay = do
  s <- get
  let pic = generateDisplay s
  vty <- ask
  liftIO $ V.update vty pic


updateUndoDisplay :: UndoPrompter s ()
updateUndoDisplay = do
  s <- get
  let pic = generateUndoDisplay s
  vty <- ask
  liftIO $ V.update vty pic


handleNextEvent :: forall s. Prompter s Bool
handleNextEvent = ask >>= liftIO . V.nextEvent >>= handleEvent
  where
    handleEvent :: Event -> Prompter s Bool
    handleEvent e = let
      c = eventToChar e in do
      Display (x,y,z) <- get
      let
        m' :: Maybe (Display s)
        m' =
            Display <$>
            ((, , )
              <$> (c >>= handleMenu x)
              <*> (c >>= handleMenu y)
              <*> (c >>= handleMenu z))
      case m' of
        Just m@(Display (r,s,t)) -> do
          put m
          return $ case r of {Quit _ -> True; _ -> False}
        Nothing -> return False
    handleMenu :: forall y. Menu y -> Char -> Maybe (Menu y)
    handleMenu (Quit _) _ = Nothing
    handleMenu (Menu _ m) c = Map.lookup c m


handleUndoNextEvent :: forall s. UndoPrompter s Bool
handleUndoNextEvent = ask >>= liftIO . V.nextEvent >>= handleEvent
  where
    handleEvent :: Event -> UndoPrompter s Bool
    handleEvent e = let
      c = eventToChar e in do
      UndoDisplay (Display (x,y,z)) <- get
      let
        m' :: Maybe (UndoDisplay s)
        m' =
            (UndoDisplay . Display) <$>
            ((, , )
              <$> (c >>= handleMenu x)
              <*> (c >>= handleMenu y)
              <*> (c >>= handleMenu z))
      case m' of
        Just m@(UndoDisplay (Display (r,s,t))) -> do
          put m
          return $ case r of {Quit _ -> True; _ -> False}
        Nothing -> return False
    handleMenu :: forall y. UndoMenu y -> Char -> Maybe (UndoMenu y)
    handleMenu (Quit _) _ = Nothing
    handleMenu (Menu _ m) c = Map.lookup c m
     

generateDisplay :: Display s  -> Picture
generateDisplay m = V.Picture V.NoCursor [renderDisplay m] V.ClearBackground

generateUndoDisplay :: UndoDisplay s -> Picture
generateUndoDisplay m = V.Picture V.NoCursor [renderUndoDisplay m] V.ClearBackground

{- STDIN prompts -}

prompt :: String -> IO String
prompt x = do
  putStrLn x
  getLine

promptYN :: String -> IO Bool
promptYN x = do
  putStrLn x
  promptYN' getChar

promptYN' :: (Monad m) => m Char -> m Bool
promptYN' c =
  fmap (`elem` ['y','Y']) $ loopUntil (`elem` ['y', 'Y', 'n', 'N']) c
  

loopUntil :: Monad m => (a -> Bool) -> m a -> m a
loopUntil p f = do
  x <- f
  if p x
    then return x
    else loopUntil p f

loopUntil_ :: Monad m => (a -> Bool) -> m a -> m ()
loopUntil_ p a = loopUntil p a >> return ()


putString :: String -> Display String
putString s = Display (trivialMenu s, pure [], trivialMenu $ draw s)


stringInput :: Display String
stringInput = Display (stringInputMenu, pure [], fmap draw stringInputMenu)

stringUndoInput :: UndoDisplay String
stringUndoInput = mkUndoDisplay stringInput

charInput :: Display Char
charInput = Display (charInputMenu, pure [], fmap draw stringInputMenu)

charUndoInput :: UndoDisplay Char
charUndoInput = mkUndoDisplay charInput

boolInput :: Display Bool
boolInput = fmap (`elem` ['y','Y','n','N']) $ loopUntil (`elem` ['y','Y','n','N']) charInput


boolUndoInput :: UndoDisplay Bool
boolUndoInput = fmap (`elem` ['y','Y','n','N']) $ loopUntil (`elem` ['y','Y','n','N']) charUndoInput


