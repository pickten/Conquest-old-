{-# LANGUAGE TupleSections #-}

module Main where
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Text.Parsec
import System.Directory
import Control.Monad.State

data Node a = Node a [Node a]
data Action = PA [Int] | IPA [(Int, Int)] | NA [Char]
data PT = PT {maxMoves :: Int, maxCarry :: Int, actions :: Map Char (Bool, Action), name::String}
data Config = Cfg (Map.Map Int (String, [Int])) [PT]

main :: IO ()
main = do
  args <- getArgs
  if args == []
    then fmap main2 $ "Please input a file to edit"
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

prompt :: String -> IO String
prompt x = do
  putStrLn x
  getLine

promptYN :: String -> IO Bool
promptYN x = do
  putStrLn x
  return $ (`elem` "yY") $ loopUntil (`elem` "yYnN") getChar

type Options = [(Char, String, IO ())]

promptOptions :: Options -> IO ()
promptOptions o = do
  let x = map (\i ->"[" ++ (first i):"] - " ++ second i) o
  

loopUntil :: (a -> Bool) -> IO a -> IO a
loopUntil p f = do
  x <- f
  if p x
    then return x
    else loopUntil p f

parseConfigFile :: String -> IO (Either String (Either ParseError Config))
parseConfigFile f = do
  s <- readFile f
  parseConfigString s f

parseConfigString :: String -> String -> IO (Either String (Either ParseError Config))
parseConfigString s f = do
  (b, r) <- expandIncludes s f
  if b
    then return $ Left r
    else return $ Right $ runParser configParser () f r

expandIncludes :: String -> String -> IO (Bool, String)
expandIncludes s f = let
  a = lines s
  b = any ((== '#') . head) a
  x = map (\t -> if (head t == '#') then do {ff <- readFile $ tail t; fmap snd $ expandIncludes ff (tail t)} else return t) a
  in fmap (b,) $ foldr (\i a -> do {y <- i; z <- a; return $ y ++ z}) (return "") x

configParser :: Parsec String (Int,Int) Config
configParser = do
  xs <- many1 (parens nodeStep <|> braces pieceStep)
  return $ foldr applyStep (Cfg Map.empty []) xs
  where
    parseInt :: Parsec String (Int,Int) Int
    parseInt = fmap read $ many1 digit
    nodeStep :: Parsec String (Int,Int) Config
    nodeStep = do
      piece <- parseInt
      lexeme
      char '\n'
      lexeme
      nodet <- anychar
      v <- many1 $ do {n <- parseInt; lexeme; char '\n'; lexeme; return n}
      (num,bl) <- get
      put (num + 1, bl)
      return (Cfg (Map.singleton num (name,v)) [])

    pieceStep :: Parsec String (Int,Int) Config
    pieceStep = do
      nam <- manyTill lexeme
      maxy <- parseInt
      lexeme
      char '\n'
      lexeme
      maxc <- parseInt
      lexeme
      char '\n'
      lexeme

      ls <- many1 parseAction
      
      (bl, num) <- get
      put (bl, num + 1)
      let p =  PT {maxMoves=maxy, maxCarry=maxc, actions=Map.fromList ls, name=nam}
      return (Cfg Map.empty [p])

    parseAction :: Parsec String (Int, Int) (Bool, Action)
    parseAction = do
      ty <- letter
      try (do {char ty; (x,y) <- rest ty; return (true, y)}) <|> rest
        where
          rest c = do
            ls <- many1 $ do {parseInt; lexeme; '\n'; lexeme}
            if c `elem` "kCmd"
              then return $ PA ls
              else if c `elem` "cr"
                   then return $ IPA $ splitter ls
                   else return $ NA $ map chr ls
          splitter [] = []
          splitter [a] = []
          splitter (a:b:c) = (a,b) : (splitter c)
            
applyStep :: Config -> Config -> Config
applyStep (Cfg m o) (Cfg l p) = Cfg (Map.union m l) (o ++ p)

manipulate :: Config -> String -> IO ()
manipulate (Cfg m o) s = do
  promptOptions configOptions
