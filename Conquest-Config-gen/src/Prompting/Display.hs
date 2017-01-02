{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Prompting.Display where
import Prompting.Menu (UndoMenu, bindUndo, Menu(..), Switch, stringInputMenu, getMenuState, charInputMenu)

import Graphics.Vty (Vty, Event, Image, Picture, Key, Modifier, (<->), (<|>))
import qualified Graphics.Vty as V
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.List (inits)


import Control.Applicative hiding ((<|>))

import Control.Arrow as Arrow
import Control.Monad.RWS
import Data.Char (toUpper)

import Data.Foldable


newtype Display' n m s = Display
  {fromDisplay ::
  (Menu s, Menu n, Menu m)} deriving (Eq, Functor)

type Display = Display' [[Text]] [Image]

type UndoHelper s = (s, [Menu s])

newtype UndoDisplay' n m s = UndoDisplay
  {fromUndoDisplay :: Display' (UndoHelper n) (UndoHelper m) (UndoHelper s)}
  deriving (Eq, Functor)

type UndoDisplay = UndoDisplay' [[Text]] [Image]

mkUndoDisplay :: Display' n m s -> UndoDisplay' n m s
mkUndoDisplay (Display (x,y,z)) = UndoDisplay $ Display (bindUndo x, bindUndo y, bindUndo z)

instance (Monoid n, Monoid m) => Applicative (Display' n m) where
  pure s = Display (pure s, pure mempty, pure mempty)
  (Display (x,y,z)) <*> (Display (x',y',z')) = Display
    (x <*> x', const <$> y' <*> (x <*> x'), const <$> z' <*> (x <*> x'))

instance (Monoid n, Monoid m) => Monad (Display' n m) where
  return = pure
  (>>=) :: forall a b. Display' n m a -> (a -> Display' n m b) -> Display' n m b
  (Display (a,b,c)) >>= f = let
    mm :: ((Menu b, Menu n, Menu m) -> Menu w) -> Menu w
    mm x = a >>= (x . fromDisplay . f)
    ff (x,y,z) = x
    ss (x, y, z) = y
    tt (x, y, z) = z
    in
    Display (mm ff, b >>= const (mm ss), c >>= const (mm tt))


instance (Monoid n, Monoid m)=>Applicative (UndoDisplay' n m) where
  pure = UndoDisplay . (\(Display (x,y,z)) -> Display (bindUndo x, bindUndo y, bindUndo z)) . pure
  (UndoDisplay (Display (x,y,z))) <*> (UndoDisplay (Display (x',y',z'))) = let
    al = fmap (\(f,g) (x,y)-> (f x, zipWith (<*>) g y))
    in UndoDisplay $ Display
    (al x <*> x', const <$> y' <*> (al x <*> x'), const <$> z' <*> (al x <*> x'))

instance (Monoid n, Monoid m)=>Monad (UndoDisplay' n m) where
  return = pure
  (>>=) :: forall a b. UndoDisplay' n m a -> (a -> UndoDisplay' n m b) -> UndoDisplay' n m b
  (UndoDisplay (Display (a,b,c))) >>= f = let
    mm :: ((UndoMenu b, UndoMenu n, UndoMenu m) -> UndoMenu w) -> UndoMenu w
    mm x = a >>= (x . fromDisplay . fromUndoDisplay . f . fst)
    ff (x,y,z) = x
    ss (x, y, z) = y
    tt (x, y, z) = z
    in
    UndoDisplay $ Display (mm ff, b >>= const (mm ss), c >>= const (mm tt))


class Rendered s where
  draw :: s -> [Image]

fromMenu :: Rendered s => Menu s -> Menu (s -> Maybe Text) -> Display s
fromMenu m tm = Display (m, makePrompt $ ($) <$> tm <*> m, fmap draw m)

makePrompt :: Menu (Maybe Text) -> Menu [[Text]]
makePrompt (Quit _) = Quit []
makePrompt  o@(Menu s m) = let
  appender :: Text -> Maybe Text -> Text
  appender x Nothing = ""
  appender x (Just y) = T.append x y
  
      
  ts = map (\c -> ("[" `T.append` (case c of {'\n' -> "<RET>"; '\t' -> "<TAB>"; _ -> T.pack [c]}) `T.append` "] - " `appender`) $ getMenuState $ m!c) $ Map.keys m
  in 
  Menu (makeColumns ts) $ fmap makePrompt m

makeColumns :: [Text] -> [[Text]]
makeColumns ts = let
  zip' :: [Text] -> [Text] -> [[Text]]
  zip' [] a = map (\i -> ["",i]) a
  zip' a [] = map (:[""]) a
  zip' (x:y) (z:w) = [x,z]:(zip' y w)
  
  app :: [Text] -> [(Text, Text)]
  app [] = []
  app [a] = [(a, "")]
  app (a:b:c) = (a,b) : (app c)
  chi :: Text -> [Text]
  chi = map (foldr (\a i -> T.append a $ T.append " " i ) "") . groupByAcc ((<37) . sum . map T.length) . T.splitOn " "
  in
  concatMap (uncurry zip' . (chi *** chi)) $ app ts


fromUndoMenu :: Rendered s => UndoMenu s -> UndoMenu (s -> Maybe Text) -> UndoDisplay s
fromUndoMenu m tm = UndoDisplay $
  Display (m,
           makeUndoPrompt $ (\(f,gs) (x,ys) -> (f x, zipWith (<*>) gs ys)) <$> tm <*> m,
           (,) <$> fmap (draw . fst) m <*> fmap (fmap (fmap draw) . snd) m) -- LIMBO! how deep can you go?

makeUndoPrompt :: UndoMenu (Maybe Text) -> UndoMenu [[Text]]
makeUndoPrompt m = (,) <$> (makePrompt $ fmap fst m) <*> fmap (fmap makePrompt . snd) m
      
      

instance Rendered String where
  draw = map (V.text V.defAttr) . map (foldr (\a i -> T.append i $ T.append " " a) "") . chunksOf 60 . T.pack
    where
      chunksOf n = groupByAcc ((<n) . sum . (map (fromIntegral . T.length))) . T.splitOn " "


groupByAcc :: ([a] -> Bool) -> [a] -> [[a]]
groupByAcc f []  = []
groupByAcc f s = (\i j -> i : (groupByAcc f j))
    (last $ takeWhile f $ inits s) $ map fst $ dropWhile (f . snd) $ zip s $ inits s

renderDisplay :: Display s -> Image
renderDisplay (Display (x,y,z)) = let
  grouper :: [Text] -> [Text]
  grouper = (map $ foldr (\a i -> a `T.append` "     " `T.append` i) "") . grouper'
  grouper' :: [Text] -> [[Text]]
  grouper' =
    let f = (\i -> sum (map (fromIntegral . T.length) i) + (5 * length i) < 80) in
    groupByAcc f

  y' = V.vertCat $ concatMap (map (V.text V.defAttr) . grouper) $ getMenuState y
  in
  V.vertCat $ y':(getMenuState z)

renderUndoDisplay :: UndoDisplay s -> Image
renderUndoDisplay (UndoDisplay (Display (a,b,c))) = renderDisplay $ Display (fmap fst a, fmap fst b, fmap fst c)

withText ::  Text -> Display s -> Display s
withText s (Display (x,Quit t,z)) = Display (x, Quit t, z)
withText s (Display (x, Menu y m, z)) = Display (x, Menu (y++(makeColumns [s])) m, z)

withImage ::  Image -> Display s -> Display s
withImage s (Display (x,z, Quit t)) = Display (x, z, Quit t)
withImage s (Display (x, z, Menu y m)) = Display (x, z, Menu (y++[s]) m)

rotary :: (a->Image) -> [a] -> Image
rotary f [] = V.text V.defAttr "~~~"
rotary f [x] = f x
rotary f [x,y] = f x <-> f y
rotary f (x:y:xs) = f (last xs) <-> f x <-> f y
