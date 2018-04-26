{-# LANGUAGE TemplateHaskell, MonadComprehensions, DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables#-}
module Shogi where

import Data.Map as Map(Map, fromList, foldlWithKey, mapWithKey, union, mapKeys, insert, lookup, null, empty, filter, filterWithKey, delete, keys, (!))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Functor(($>), (<$))
import Control.Lens ((%~), _1, _2, (.~), makeLenses, (&), both, (^.), use, (.=), (%=))
import Control.Monad (mplus, guard, forM_, MonadPlus, mzero, msum)
import Control.Monad.State (get, put, StateT(..), State, runStateT, evalStateT, execStateT)
import qualified Data.List as List(delete)
import Data.Monoid((<>))
import Text.Parsec as Parsec

data Piece' a =
  King
  | Pawn a
  | Knight a
  | Lance a -- 香車
  | Gold
  | Silver a
  | Bishop a
  | Rook a
  deriving (Eq, Functor, Read, Show)

type Piece = Piece' Promotion

showPiece :: Piece -> String
showPiece King = "玉"
showPiece Gold = "金"
showPiece (Silver Unpromoted) = "銀"
showPiece (Silver Promoted) = "全"
showPiece (Knight Unpromoted) = "桂"
showPiece (Knight Promoted) = "圭"
showPiece (Lance Unpromoted) = "香"
showPiece (Lance Promoted) = "杏"
showPiece (Pawn Unpromoted) = "歩"
showPiece (Pawn Promoted) = "と"
showPiece (Rook Unpromoted) = "飛"
showPiece (Rook Promoted) = "竜"
showPiece (Bishop Unpromoted) = "角"
showPiece (Bishop Promoted) = "馬"

data Promotion = Promoted | Unpromoted deriving (Eq,Show,Read)
data Direction = Top | Par | Subtraction | DirLeft | DirRight deriving (Eq,Show,Read)
data Move = Move Piece (Int, Int) [Direction] Promotion deriving (Eq,Show,Read) -- 指し手

data Turn = First | Later deriving (Eq,Ord,Read,Show) -- 手番

data Square = Square {_sqPiece::Piece, _sqTurn :: Turn} deriving (Eq,Read,Show)

makeLenses ''Square

data Field = Field { _fromField :: Map.Map (Int,Int) Square,
                     _caputured :: Map.Map Turn [Piece]} deriving (Eq,Show,Read) -- コモナド?

makeLenses ''Field


showSquare :: Square -> String
showSquare (Square King Later) = "g王"
showSquare (Square King First) = " 玉"
showSquare (Square pie Later) = "g"++ showPiece pie
showSquare (Square pie First) = " " ++ showPiece pie


showField :: Field -> String
showField (Field mp captured) =
    let showDan dan mp = (showRowGrid [fromMaybe "　 " $ (i, dan) `Map.lookup` mp | i <- [-1..10]])
      in let danScale = fromList [((0, n), show $ ChineseNumber n) | n <- [1..9]]-- 目盛り
        in let (cap::Map (Int, Int) String) = fromList $ [((-1, n), showPiece $ (captured Map.! First) !! n) | n <- [0..(length (captured Map.! First))-1], 0 <= n] <> [((10,n), showPiece $ (captured Map.! Later) !! n) | n <- [0..(length (captured Map.! Later))-1], 0 <= n] --持ち駒
          in let sujiScale = fromList [((n, 0), show n ++ "　") | n <- [1..9]]-- 目盛り
            in showColumnGrid $ (map (showDan `flip` (fmap showSquare mp `union` danScale `union` sujiScale `union` cap)) [0..9])


showRowGrid :: [String] -> String
showRowGrid = foldl (\str strs -> strs ++ "|" ++ str) ""

showColumnGrid :: [String] -> String
showColumnGrid = foldl
  (\str strs -> str ++ "\n" ++ ("   " ++ replicate (9*2) '―') ++ "\n" ++ strs)
  ""

newtype ChineseNumber = ChineseNumber { fromChineseNumber :: Int}

instance Show ChineseNumber where
  show (ChineseNumber 1) = "一"
  show (ChineseNumber 2) = "ニ"
  show (ChineseNumber 3) = "三"
  show (ChineseNumber 4) = "四"
  show (ChineseNumber 5) = "五"
  show (ChineseNumber 6) = "六"
  show (ChineseNumber 7) = "七"
  show (ChineseNumber 8) = "八"
  show (ChineseNumber 9) = "九"
  show _ = "Supported Kan number is [1-9]."


symmetry :: Int -> Int
symmetry x = 10 - x

danSymmetry :: (Int, Int) -> (Int, Int)
danSymmetry = _1 %~ symmetry

sujiSymmetry :: (Int, Int) -> (Int, Int)
sujiSymmetry = _2 %~ symmetry

fifiPSymmetry :: (Int, Int) -> (Int, Int) -- (5,5)点対称
fifiPSymmetry = danSymmetry . sujiSymmetry

danSymList :: [((Int, Int), Square)] -> [((Int, Int), Square)]
danSymList = foldl (\xs (loc, pie) -> (danSymmetry loc, pie):((loc, pie):xs)) []

bothSymMap :: Map.Map (Int, Int) Square -> Map.Map (Int, Int) Square
bothSymMap = foldlWithKey (\xs loc pie -> insert (fifiPSymmetry loc) (pie & sqTurn %~ turnChange) xs) Map.empty

reverseField :: Field -> Field
reverseField (Field fie mochi) = Field (foldlWithKey (\xs loc pie -> insert (fifiPSymmetry loc) (pie & sqTurn %~ turnChange) xs) Map.empty fie) $ fromList [(First, mochi Map.! Later), (Later, mochi Map.! First)]

initialField :: Field
initialField = let later_field = pawnList `Map.union` symmetric_part `Map.union` unsym_part
                   in Field (later_field `union` bothSymMap later_field) $ fromList [(First, []), (Later, [])]
 where
   piece x = Square x Later
   pawnList = fromList $ [((n, 3), piece $ Pawn Unpromoted) | n <- [1..9]]

   symmetric_part = mapKeys danSymmetry right_of_sym `Map.union` right_of_sym
   right_of_sym = fromList $ [
     ((1,1), piece $ Lance Unpromoted)
     ,((2,1), piece $ Knight Unpromoted)
     ,((3,1), piece $ Silver Unpromoted)
     ,((4,1), piece $ Gold)
     ,((5,1), piece King)
     ]
   unsym_part = fromList $ [
     ((2,2), piece $ Bishop Unpromoted)
     ,((8,2), piece $ Rook Unpromoted)
     ]

moveMap :: Ord i => i -> i -> Map.Map i a -> Map.Map i a
moveMap i j mp = let a = mp ! i in insert j a (Map.delete i mp)

moveMapZero :: (Ord i, MonadPlus m) => i -> i -> Map.Map i a -> m (Map.Map i a)
moveMapZero i j mp = let x = Map.lookup i mp in
  case x of
    Just a -> return $ insert j a (Map.delete i mp)
    Nothing -> mzero

applyPiece :: PieceM (Int, Int) -> PieceM (Either (Piece, (Int, Int)) (Int, Int))
applyPiece f = do
  original_xy <- use _1
  xy <- f
  _1 .= xy
  original_field <- use $ _2 . fromField
  case Map.lookup xy original_field of
     Just (Square pie First) -> mzero
     Just (Square pie Later) -> do
       field <- moveMapZero original_xy xy original_field
       _2 %= (\(Field _ cap) -> Field field $ Map.insert First (pie:(cap Map.! First)) cap)
       return $ Left (pie, xy)
     Nothing -> do
       field <- moveMapZero original_xy xy original_field
       _2 %= (\(Field _ cap) -> Field field cap)
       return $ Right xy

right1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
right1 = applyPiece [(x-1, y) | (x, y) <- use _1, 0 < x-1]

left1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
left1 = applyPiece [(x+1, y) | (x,y) <- use _1, x+1 <= 9]

up1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
up1 = applyPiece [(x, y-1) | (x, y) <- use _1, 0 < y-1]

down1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
down1 = applyPiece [(x, y+1) | (x, y) <- use _1, y+1 <= 9 ]

moveAll :: PieceM (Either (Piece, (Int, Int)) (Int, Int)) -> PieceM (Int, Int)
moveAll move1 = moveAll' `mplus` use _1 where
  moveAll' = do
    loc2 <- move1
    case loc2 of
      Left (pie, xy) -> do
        _1 .= xy
        return (xy::(Int, Int))
      Right xy -> do
        _1 .= xy
        moveAll move1

height1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
height1 = up1 `mplus` down1

width1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
width1 = right1 `mplus` left1

corner1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
corner1 = height1 >> width1

cornerAll :: PieceM (Int, Int)
cornerAll =
  moveAll (right1 >> up1)
  `mplus` moveAll (left1 >> up1)
  `mplus` moveAll (right1 >> down1)
  `mplus` moveAll (left1 >> down1)

widthAll :: PieceM (Int, Int)
widthAll = moveAll right1 `mplus` moveAll left1

upAll :: PieceM (Int, Int)
upAll = moveAll up1

heightAll :: PieceM (Int, Int)
heightAll = upAll `mplus` moveAll down1


type PieceM = StateT ((Int, Int), Field) [] -- 駒モナド
-- ロールバック

eitherPoint :: Either (Piece, (Int, Int)) (Int, Int) -> (Int, Int)
eitherPoint = either (^. _2) id

movable :: Piece -> PieceM (Int, Int)
movable King = do
  loc1 <- use _1
  ((fmap eitherPoint height1) `mplus` return loc1)
  loc2 <- (fmap eitherPoint width1) `mplus` use _1
  guard (loc1 /= loc2)
  return loc2
movable (Rook Unpromoted) = do
  loc1 <- use _1
  loc2 <- heightAll `mplus` widthAll
  guard (loc1 /= loc2)
  return loc2
movable (Rook Promoted) = movable (Rook Unpromoted) `mplus` movable King
movable (Bishop Unpromoted) = do
  loc1 <- use _1
  loc2 <- cornerAll
  guard (loc1 /= loc2)
  return loc2
movable (Bishop Promoted) = movable (Bishop Unpromoted) `mplus` movable King
movable Gold = do
  (loc1::(Int, Int)) <- use _1
  (loc2::(Int,Int)) <- (up1 >> ((fmap eitherPoint width1) `mplus` use _1))
    `mplus` ((fmap eitherPoint width1) `mplus` use _1) `mplus` fmap eitherPoint down1
  guard (loc1 /= loc2)
  return loc2
movable (Silver Unpromoted) = do
  loc1 <- use _1
  loc2 <- fmap eitherPoint up1 `mplus` fmap eitherPoint corner1
  guard (loc1 /= loc2)
  return loc2
movable (Silver Promoted) = movable Gold
movable (Knight Unpromoted) = do
  loc1 <- use _1
  loc2 <- [ (x+1, y+3) | (x, y) <- use _1 ] `mplus` [ (x-1, y+3) | (x, y) <- use _1 ]
  guard (loc1 /= loc2)
  return loc2
movable (Knight Promoted) = movable Gold
movable (Lance Unpromoted) = do
  loc1 <- use _1
  loc2 <- upAll
  guard (loc1 /= loc2)
  return loc2
movable (Lance Promoted) = movable Gold
movable (Pawn Unpromoted) = fmap eitherPoint up1
movable (Pawn Promoted) = movable Gold

promotion :: Piece -> Piece
promotion = (Promoted <$)

move :: Move -> Field -> [Field]
move (Move pie loc dirs is_prom) field =
  let pies = keys $ Map.filter (==(Square pie First)) $ (^.fromField) field in
  let ex = mapMaybe (\k -> if loc `elem` (map (^._1) $ execStateT (movable pie) (k, field)) then Just $ head $ Prelude.filter (\(l,_) -> l == loc) $ execStateT (movable pie) (k, field) else (Nothing::Maybe ((Int,Int), Field))) pies in
  map reverseField $ case ex of
    [] -> []
    xs -> map (^. _2) xs


-- 毎回盤をひっくり返すことで手番を表せる

turnChange :: Turn -> Turn
turnChange First = Later
turnChange Later = First

shogiTest :: String
shogiTest = (concat $ map showField $ Shogi.move (Shogi.Move (Shogi.King) (4, 8) [] Shogi.Unpromoted) $ Shogi.initialField)
        <> "\n" <> (concat $ map showField $ map (\((x1,y1),fie) -> fie) $ execStateT (Shogi.movable (Shogi.Lance Shogi.Unpromoted)) ((2, 7), initialField))
        <> "\n" <> (concat $ map (showField . (^._2)) $ execStateT (Shogi.movable King) ((5, 9), head $ move (Shogi.Move Gold (5, 8) [] Shogi.Unpromoted) $ head $ move (Shogi.Move Gold (5, 8) [] Shogi.Unpromoted) $ initialField))

-- TODO: 斜めの動きがおかしい
--
chineseNumParser :: (Monad m) => ParsecT String u m Int
chineseNumParser = do
  c <- msum $ map char "一二三四五六七八九"
  return $ case c of
    '一' -> 1
    'ニ' -> 2
    '三' -> 3
    '四' -> 4
    '五' -> 5
    '六' -> 6
    '七' -> 7
    '八' -> 8
    '九' -> 9

pieceParser :: (Monad m) => ParsecT String u m Shogi.Piece
pieceParser = do
  nari <- Shogi.Promoted <$ (char '成') <|> return Shogi.Unpromoted
  str <- msum $ map string ["歩", "香", "香車", "桂", "桂馬", "銀", "金", "玉","王", "飛", "飛車", "角"]

  let unpromoteds = case str of "歩" -> Shogi.Pawn Shogi.Unpromoted
                                "香" -> Shogi.Lance  Shogi.Unpromoted
                                "香車" -> Shogi.Lance  Shogi.Unpromoted
                                "桂" -> Shogi.Knight Shogi.Unpromoted
                                "桂馬" -> Shogi.Knight Shogi.Unpromoted
                                "銀" -> Shogi.Silver Shogi.Unpromoted
                                "金" -> Shogi.Gold
                                "玉" -> Shogi.King
                                "王" -> Shogi.King
                                "飛" -> Shogi.Rook Shogi.Unpromoted
                                "飛車" -> Shogi.Rook Shogi.Unpromoted
                                "角" -> Shogi.Bishop Shogi.Unpromoted

  return $ case nari of
    Shogi.Promoted -> Shogi.promotion unpromoteds
    Shogi.Unpromoted -> unpromoteds


moveParser :: (Monad m) => ParsecT String u m (Shogi.Move)
moveParser = Parsec.try $ do
  n <- (read<$>(msum $ map (fmap (\x->[x]) . char) "123456789")) <|> chineseNumParser
  m <- (read<$>(msum $ map (fmap (\x->[x]) . char) "123456789")) <|> chineseNumParser
  piece <- pieceParser
  isPromoted <- (Shogi.Promoted <$ (char '成')) <|> (Shogi.Unpromoted <$ (string "不成")) <|> return Shogi.Unpromoted
  return $ Shogi.Move piece (n,m) [] isPromoted
