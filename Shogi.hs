{-# LANGUAGE TemplateHaskell, MonadComprehensions, DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables#-}
module Shogi where

import Data.Map as Map(Map, fromList, foldlWithKey, mapWithKey, union, mapKeys, insert, lookup, null, empty, filter, filterWithKey, delete, keys, (!))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Functor(($>), (<$))
import Control.Lens ((%~), _1, _2, (.~), makeLenses, (&), both, (^.), use, (.=), (%=))
import Control.Monad (mplus, guard, forM_, MonadPlus, mzero)
import Control.Monad.State (get, put, StateT(..), State, runStateT, evalStateT, execStateT)
import qualified Data.List as List(delete)
import Data.Monoid((<>))

data Piece' a =
  King
  | Pawn a
  | Knight a
  | Lance a -- 香車
  | Gold
  | Silver a
  | Bishop a
  | Rook a
  deriving (Eq, Functor)

type Piece = Piece' Promotion

instance Show Piece where
  show King = "玉"
  show Gold = "金"
  show (Silver Unpromoted) = "銀"
  show (Silver Promoted) = "全"
  show (Knight Unpromoted) = "桂"
  show (Knight Promoted) = "圭"
  show (Lance Unpromoted) = "香"
  show (Lance Promoted) = "杏"
  show (Pawn Unpromoted) = "歩"
  show (Pawn Promoted) = "と"
  show (Rook Unpromoted) = "飛"
  show (Rook Promoted) = "竜"
  show (Bishop Unpromoted) = "角"
  show (Bishop Promoted) = "馬"


data Promotion = Promoted | Unpromoted deriving (Eq,Show)
data Direction = Top | Par | Subtraction | DirLeft | DirRight deriving (Eq,Show)
data Move = Move Piece (Int, Int) [Direction] Promotion deriving (Eq,Show) -- 指し手

data Turn = First | Later deriving (Eq,Ord) -- 手番

data Square = Square {_sqPiece::Piece, _sqTurn :: Turn} deriving Eq

makeLenses ''Square

data Field = Field { _fromField :: Map.Map (Int,Int) Square,
                     _caputured :: Map.Map Turn [Piece]} deriving Eq -- コモナド?

makeLenses ''Field
instance Show Square where
  show (Square King Later) = "g王"
  show (Square King First) = " 玉"
  show (Square pie Later) = "g"++ show pie
  show (Square pie First) = " " ++ show pie

instance Show Field where
  show (Field mp captured) =
    let showDan dan mp = (showRowGrid [fromMaybe "　 " $ (i, dan) `Map.lookup` mp | i <- [-1..10]])
      in let danScale = fromList [((0, n), show $ ChineseNumber n) | n <- [1..9]]-- 目盛り
        in let (cap::Map (Int, Int) String) = fromList $ [((-1, n), show $ (captured Map.! First) !! n) | n <- [0..(length (captured Map.! First))-1], 0 <= n] <> [((10,n), show $ (captured Map.! Later) !! n) | n <- [0..(length (captured Map.! Later))-1], 0 <= n] --持ち駒
          in let sujiScale = fromList [((n, 0), show n ++ "　") | n <- [1..9]]-- 目盛り
            in showColumnGrid $ (map (showDan `flip` (fmap show mp `union` danScale `union` sujiScale `union` cap)) [0..9])


showRowGrid :: [String] -> String
showRowGrid = foldl (\str strs -> strs ++ "|" ++ str) ""

showColumnGrid :: [String] -> String
showColumnGrid = foldl
  (\str strs -> str ++ "\n" ++ ("   " ++ replicate (9*4) '―') ++ "\n" ++ strs)
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
  (fmap eitherPoint width1) `mplus` use _1
  loc2 <- (fmap eitherPoint height1) `mplus` use _1
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
  let ex = mapMaybe (\k -> if loc `elem` (evalStateT (movable pie) (k, field)) then Just $ head $ Prelude.filter (\(l,_) -> l == loc) $ execStateT (movable pie) (k, field) else (Nothing::Maybe ((Int,Int), Field))) pies in
  case ex of
    [] -> []
    xs -> map (^. _2) xs


-- 毎回盤をひっくり返すことで手番を表せる

turnChange :: Turn -> Turn
turnChange First = Later
turnChange Later = First

shogiTest :: String
shogiTest = (show $ map reverseField $ Shogi.move (Shogi.Move (Shogi.Rook Shogi.Unpromoted) (3, 8) [] Shogi.Unpromoted) $ Shogi.initialField)
        <> "\n" <> (show $ map (\((x1,y1),fie) -> reverseField fie) $ execStateT (Shogi.movable (Shogi.Lance Shogi.Unpromoted)) ((2, 7), initialField))
