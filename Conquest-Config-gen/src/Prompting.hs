module Prompting where
import Parsing

{- EXPERIMENTAL STUFF -}

import Graphics.Vty (Vty, Event, Picture, Key, Modifier)
import qualified Graphics.Vty as V

import Control.Applicative hiding ((<|>))
import Control.Arrow
import Control.Monad.RWS
import Data.Char (toUpper)

import Data.Default (def)
import Data.Sequence (Seq, (<|) )
import qualified Data.Sequence as Seq
import Data.Foldable

eventToChar :: Event -> Maybe Char
eventToChar (V.EvKey k xs) = keyToChar k xs
  where
    keyToChar :: Key -> [Modifier] -> Maybe Char
    keyToChar k [] = unModKTC k
    keyToChar k [V.MShift] = capsKTC k
    keyToChar _ _ = Nothing

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

type App = RWST Vty () (Seq String) IO

{- NONEXPERIMENTAL STUFF (What we currently use) -}

prompt :: String -> IO String
prompt x = do
  putStrLn x
  getLine

promptYN :: String -> IO Bool
promptYN x = do
  putStrLn x
  fmap (`elem` "yY") $ loopUntil (`elem` "yYnN") getChar

type Options = [(Char, (String, IO ()))]

makeOptions :: [(Char, String, IO ())] -> Options
makeOptions = map (\(a,b,c) -> (a, (b,c)))

promptOptions :: Options -> IO ()
promptOptions o = let
    x :: [String]
    x = map (\i ->"[" ++ (fst i):"] - " ++ fst (snd i)) o
    y :: Int
    y = maximum $ map length x
    z :: Int
    z = 80 `div` (y+5)
    cc :: [String]
    cc = foldr (\i a -> zipWith (\s t -> s ++ "     " ++ t) a i) (replicate z "") $ chunksOf z x
  in
  loopUntil_ id $ do
    mapM_ putStrLn cc

    c <- getChar
    putStrLn ""
    maybe (return False) (>> return True) $ fmap snd $ lookup c o
  
  where
    chunksOf n y  | length y < n = []
    chunksOf n xs = (take n xs) : (chunksOf n $ drop n xs)
  
  

loopUntil :: (a -> Bool) -> IO a -> IO a
loopUntil p f = do
  x <- f
  if p x
    then return x
    else loopUntil p f

loopUntil_ :: (a -> Bool) -> IO a -> IO ()
loopUntil_ p a = loopUntil p a >> return ()
