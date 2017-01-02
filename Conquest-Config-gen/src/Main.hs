{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prompting.Display
import Prompting
import Parsing
import Data.Map ((!), Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Text.Parsec  as T hiding (State)
import System.Directory
import System.Environment
import Control.Monad.State
import Data.Maybe (isJust)
import Control.Applicative ((<$>), (<*>))
import qualified Control.Applicative as A
import qualified Graphics.Vty as V
import Text.Regex.Posix ((=~), Regex)
import qualified Text.Regex.Posix as R -- for regex
import Text.Regex.Base.RegexLike (makeRegexM)

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
                    putStrLn "Alternatively, you could try the debugging tactics listed in <../README>"
                  Right c' -> manipulate c' f
            else manipulate (Cfg Map.empty []) f

manipulate :: Config -> String -> IO ()
manipulate oc f = do
  (m,c) <- runMainUndoMenu $ runStateT mainMenu oc
  case m of
    Just ff -> T.writeFile (if ff == "" then f else ff) (serializeConfig c)
    Nothing -> return ()
  where
    
withText' = mapStateT . withText
withImage' = mapStateT . withImage

mainMenu :: StateT Config UndoDisplay (Maybe String)
mainMenu = do
  mapStateT mkUndoDisplay editConfig
  k <- lift boolUndoInput
  if k
    then do
    s <- lift stringUndoInput
    return $ Just s
    else return Nothing
        
editConfig :: StateT Config Display ()
editConfig = do
  c <- lift charInput
  case c of
    't' -> withText' "Edit piece types" editTypes
    'n' -> withText' "Edit the map" editNodes
    '?' -> withText' "Help" $ do
      displayHelp
      lift charInput
      editConfig
    _ -> editConfig

editTypes :: StateT Config Display ()
editTypes = do
  c <- lift charInput
  cfg@(Cfg m cf) <- get
  withImage' (rotary pieceDisplay cf) $ do
  case (c, cf) of
    ('\n', []) -> withText' "New piece type" $  do
      cf' <- fmap ((Cfg m) . (:[])) editNewType
      put cf'
      editTypes
            
    ('\n', (x:xs)) -> withText' "Edit" $ do
      x' <- editPieceType x
      put $ Cfg m $ x':xs
      editTypes
              
    ('h', (x:xs)) -> withText' "Rotate right" $ do
      put $ Cfg m $ xs ++ [x]
      editTypes
          
    ('j', (x:xs)) -> withText' "Rotate right" $  do
      put $ Cfg m $ xs ++ [x]
      editTypes
          
    ('k', xs@(_:_)) -> withText' "Rotate left" $ do
      put $ Cfg m $ (last xs):(init xs)
      editTypes

    ('l', xs@(_:_)) -> withText' "Rotate left" $ do
      put $ Cfg m $ (last xs):(init xs)
      editTypes

    ('x', _) -> withText' "Go back to main menu" $ editConfig

    ('n', xs) -> withText' "New piece type" $ do
      cf' <- fmap ((Cfg m) . (:xs)) editNewType
      put cf'
      editTypes

    ('/', xs) -> do
      cf' <- searchType >>= editPieceType
      put $ Cfg m $ map (\y-> if ptID cf' == ptID y then cf' else y) xs
      editTypes
        
    _ -> editTypes

selectType :: StateT Config Display PT
selectType = do
  cfg@(Cfg m cf) <- get
  rotaryFromSearch pieceDisplay cf searchType

selectNode :: StateT Config Display Int
selectNode = do
  c <- lift charInput
  cfg@(Cfg m cf) <- get
  withImage' (rotary intDisplay $ Map.keys m) $ rotaryFromSearch intDisplay (Map.keys m) searchNode



editNewType :: StateT Config Display PT
editNewType = do
  Cfg _ xs <- get
  return $ PT (length xs) $ PT' 0 0 Map.empty ""
  
editPieceType :: PT -> StateT Config Display PT
editPieceType p = get >>= (\y -> withImage' (fullTypeDisplay y p) $ do
  c <- lift charInput
  case c of
    'k' -> withText' "Edit kill" $ editAction c p
    'c' -> withText' "Edit carry" $ editAction c p
    'C' -> withText' "Edit combo" $ editAction c p
    'r' -> withText' "Edit recapture" $ editAction c p
    't' -> withText' "Edit travel" $ editAction c p
    'm' -> withText' "Edit mount kills" $ editAction c p
    'd' -> withText' "Edit dismount kills" $ editAction c p
    _ -> editPieceType p)

editAction :: Char -> PT -> StateT Config Display PT
editAction c p = let act = maybe (defaultAction ! c) id $ Map.lookup c (actions p) in do
  act' <- editAction' act
  let acts' = Map.insert c act' (actions p)
  let ptMain' = (ptMain p) {actions' = acts'}
  return $ p {ptMain = ptMain'}
  where
    defaultAction = Map.fromList $
      [(c, (True, PA [])) | c <- "kCmd"] ++
      [(c, (False, IPA [])) | c <- "c"] ++
      [(c, (True, IPA [])) | c <- "r"] ++
      [(c, (True, NA [])) | c <- "t"]
      
    editAction'   :: (Bool, Action) -> StateT Config Display (Bool, Action)
    editAction' (b,a) = do
      c <- lift charInput
      let b = case a of {IPA _ -> True; _ -> False}
      case c of
        'q' -> return (b, a)
        'o' -> withText' "Toggle default state" $ editAction' (not b, a)
        'n' -> withText' (if b then "Add/update int for exception" else "Add new exception") $ do
          case a of
            NA xs -> do
              n <- lift charInput
              editAction' (b, NA $ if n `elem` xs then xs else n:xs)
            PA xs -> do
              n <- selectType
              editAction' (b, PA $ if (ptID n) `elem` xs then xs else (ptID n):xs)
            IPA xs -> do
              n <- selectType
              i <- lift intInput
              editAction' $ (b, ) $ IPA $ map (\y -> if (ptID n) == fst y then (fst y, i) else y) xs
        'r' -> withText' "Remove exception" $ do
          case a of
            NA xs -> do
              n <- lift charInput
              editAction' (b, NA $ filter (/= n) xs)
            PA xs -> do
              n <- selectType
              editAction' (b, PA $ filter (/= ptID n) xs)
            IPA xs -> do
              n <- selectType
              editAction' (b, IPA $ filter ((/= ptID n) . fst) xs)
        _ -> editAction' (b, a)


editNodes :: StateT Config Display ()
editNodes = do
  cfg@(Cfg m cf) <- get
  runFrom $ Map.keys m
  where
    runFrom cf = do
      c <- lift charInput
      withImage' (rotary intDisplay cf) $ do
        case (c, cf) of
          ('\n', []) -> withText' "New piece type" $  do
            editNewNode 0
            editNodes
            
          ('\n', (x:xs)) -> withText' "Edit" $ do
            editNode x
            editNodes
              
          ('h', (x:xs)) -> withText' "Rotate right" $ do
            runFrom $ xs ++ [x]
          
          ('j', (x:xs)) -> withText' "Rotate right" $  do
            runFrom $ xs ++ [x]
          
          ('k', xs@(_:_)) -> withText' "Rotate left" $ do
            runFrom $ (last xs):(init xs)

          ('l', xs@(_:_)) -> withText' "Rotate left" $ do
            runFrom $ (last xs):(init xs)

          ('x', _) -> withText' "Go back to main menu" $ editConfig

          ('n', xs) -> withText' "New piece type" $ do
            editNewNode $ length xs
            editNodes

          ('/', xs) -> do
            searchNode >>= editNode
            editNodes
          
          _ -> editNodes

editNode :: Int -> StateT Config Display ()
editNode n = do
  Cfg m cf <- get
  let old@(own, typ, thistype, con) = m ! n
  withImage' (nodeDisplay cf n old) $ do
  c <- lift charInput
  case c of
    'o' -> withText' "Change owner" $ do
      i <- withText' "Write -1 to remove the piece" $ lift intInput
      put $ flip Cfg cf $ Map.insert n (i, typ, thistype, con) m
      editNode n

    'p' -> withText' "Change contents" $ do
      p <- selectType
      put $ flip Cfg cf $ Map.insert n (own, ptID p, thistype, con) m
      editNode n

    'r' -> withText' "Remove link" $ do
      i <- selectNode
      put $ flip Cfg cf $ Map.insert n (own, typ, thistype, filter (/= i) con) m
      editNode n
    'a' -> withText' "Add link" $ do
      i <- selectNode
      put $ flip Cfg cf $ Map.insert n (own, typ, thistype, i:con) m

    't' -> withText' "Change node type" $ do
      i <- lift charInput
      put $ flip Cfg cf $ Map.insert n (own, typ, i, con) m
    
    'x' -> withText' "Back" $ return ()
    _ -> editNode n

editNewNode :: Int -> StateT Config Display ()
editNewNode i = do
  modify $ (\(Cfg a b) -> flip Cfg b $ Map.insert i (-1, 0, 'l', []) a)
  editNode i

displayHelp :: StateT Config Display ()
displayHelp = withImage' helpImage $ 
  return ()
  where
    helpImage :: V.Image
    helpImage = V.vertCat $ map (V.text V.defAttr) $ concat $ makeColumns $ (:[]) $
      foldr (\a i -> a `T.append` "\n" `T.append` i) "" helpText

    helpText :: [T.Text]
    helpText =
      ["Help"
      ,"----"
      ,""
      ,"This section intentially left blank."
      ,"Seriously, this should be reasonably straightforward to use."
      ,"If it isn\'t, feel free to check <https://github.com/pickten/Conquest>"
      ,"and make an issue if there isn\'t one to fix this."]

    
pieceDisplay :: PT -> V.Image
pieceDisplay x = V.text V.defAttr $
  (T.pack $ show $ ptID x) `T.append` " - " `T.append` name x


searchType :: StateT Config Display PT
searchType = withText' "Search" $ searchFrom "" ""
  where
    getRegex     :: String -> String -> String
    getRegex r s = case (makeRegexM :: String -> Maybe Regex) s of
      Nothing -> r
      Just _ -> s
    
    searchFrom     :: String -> String -> StateT Config Display PT
    searchFrom r s = do
      Cfg _ xs <- get
      let rr  = getRegex r s
      let xos = filter ((=~ rr) . T.unpack . name) xs
      withImage' (rotary pieceDisplay xos) $ do
        c <- lift charInput
        case (c, xos) of
          ('\n', (x:xs)) -> withText' "End search" $ rotaryFrom pieceDisplay xos

          ('\n', _) -> withText' "New search" $ searchFrom "" ""
          
          (c, _) -> searchFrom rr $ s ++ [c]


searchNode :: StateT Config Display Int
searchNode = withText' "Search" $ searchFrom "" ""
  where
    getRegex     :: String -> String -> String
    getRegex r s = case (makeRegexM :: String -> Maybe Regex) s of
      Nothing -> r
      Just _ -> s
    
    searchFrom     :: String -> String -> StateT Config Display Int
    searchFrom r s = do
      Cfg xs' _ <- get
      let xs  = Map.keys xs'
      let rr  = getRegex r s
      let xos = filter ((=~ rr) . show) xs
      withImage' (rotary intDisplay xos) $ do
        c <- lift charInput
        case (c, xos) of
          ('\n', (x:xs)) -> withText' "End search" $ rotaryFrom intDisplay xos

          ('\n', _) -> withText' "New search" $ searchFrom "" ""
          
          (c, _) -> searchFrom rr $ s ++ [c]

intDisplay   :: Int -> V.Image
intDisplay c = V.text V.defAttr $ T.pack $ show c


rotaryFrom :: (a -> V.Image) ->  [a] -> StateT Config Display a
rotaryFrom f cf = do
  c <- lift charInput
  withImage' (rotary f cf) $ do
  case (c, cf) of
    ('\n', (x:xs)) -> withText' "Select" $ return x
              
    ('h', (x:xs)) -> withText' "Rotate right" $ do
      rotaryFrom f $ xs ++ [x]
          
    ('j', (x:xs)) -> withText' "Rotate right" $  do
      rotaryFrom f $ xs ++ [x]
          
    ('k', xs@(_:_)) -> withText' "Rotate left" $ do
      rotaryFrom f $ (last xs):(init xs)

    ('l', xs@(_:_)) -> withText' "Rotate left" $ do
      rotaryFrom f $ (last xs):(init xs)

    _ -> rotaryFrom f cf


rotaryFromSearch :: (a -> V.Image) ->  [a] -> StateT Config Display a -> StateT Config Display a
rotaryFromSearch f cf g = do
  c <- lift charInput
  withImage' (rotary f cf) $ do
  case (c, cf) of
    ('\n', (x:xs)) -> withText' "Select" $ return x
              
    ('h', (x:xs)) -> withText' "Rotate right" $ do
      rotaryFromSearch f (xs ++ [x]) g
          
    ('j', (x:xs)) -> withText' "Rotate right" $  do
      rotaryFromSearch f (xs ++ [x]) g
          
    ('k', xs@(_:_)) -> withText' "Rotate left" $ do
      rotaryFromSearch f ((last xs):(init xs)) g

    ('l', xs@(_:_)) -> withText' "Rotate left" $ do
      rotaryFromSearch f ((last xs):(init xs)) g

    ('/', xs) -> g
    
    _ -> rotaryFromSearch f cf g


nodeDisplay :: [PT] -> Int -> (Int, Int, Char, [Int]) -> V.Image
nodeDisplay pts i (-1, _, typ, xs) = V.vertCat $ map (V.text V.defAttr) $ concat $
  makeColumns ["Node " `T.append` (T.pack $ show i) `T.append` " of type \"" `T.append` (T.pack [typ]),
               "Has no piece on it", "Linked to: " `T.append` (T.pack $ show i)]
nodeDisplay pts i (j, typ, types, xs) = V.vertCat $ map (V.text V.defAttr) $ concat $
  makeColumns ["Node " `T.append` (T.pack $ show i) `T.append` " of type \"" `T.append` (T.pack [types]),
               "Has a piece on it of type \"" `T.append` (name $ pts !! typ) `T.append` "\" belonging to player " `T.append` (T.pack $ show j),
               "Linked to: " `T.append` (T.pack $ show i)]


fullTypeDisplay :: Config -> PT -> V.Image
fullTypeDisplay (Cfg nodes pts) pt = V.vertCat $ map (V.text V.defAttr) $ concat $
  makeColumns $ (concatMap (renderAction $ actions pt) $ Map.keys $ actions pt) ++ [name pt, "========="]
  where
    renderAction :: Map Char (Bool, Action) -> Char -> [T.Text]
    renderAction m c =
      (actionName c):
      ("---"):(renderAction' $ m ! c)

    renderAction' :: (Bool, Action) -> [T.Text]
    renderAction' (b, a) = ("Defaults to being " `T.append` (if b then "possible" else "impossible")):(renderAction'' a)

    renderAction''         :: Action -> [T.Text]
    renderAction'' (PA xs) = ("Exceptions:"):(map (name . (pts !!)) xs)
    renderAction'' (IPA xs) = ("Exceptions:"):
      (map (\(a,b) -> (name $ (pts !! b)) `T.append` "(" `T.append` T.pack (show b) `T.append` ")") xs)
    renderAction'' (NA xs) = ("Exceptions:"):
      (map (T.pack . (:[])) xs)

    actionName c = case c of
      'k' -> "Kill"
      'c' -> "Carry"
      'C' -> "Combo"
      'r' -> "Recapture"
      't' -> "Travel"
      'm' -> "Mount Kills"
      'd' -> "Dismount Kills"
      _   -> "Unknown. Please report this as a bug" 
