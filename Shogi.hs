{-# LANGUAGE TemplateHaskell #-}
module Shogi where

import Data.Map as Map(Map, fromList, foldlWithKey, mapWithKey, union, mapKeys, insert, lookup, null, empty)
import Data.Maybe (fromMaybe)
import Control.Lens ((%~), _1, _2, (.~), makeLenses, (&))

data Piece =
  King
  | Pawn Promotion
  | Knight Promotion
  | Lance Promotion -- 香車
  | Gold
  | Silver Promotion
  | Bishop Promotion
  | Rook Promotion
data Promotion = Promoted | Unpromoted
data Direction = Top | Par | Subtraction | Left | Right
data Move = Move Piece (Int, Int) Promotion -- 指し手

data Turn = First | Later -- 手番

data Square = Square {_sqPiece::Piece, _sqTurn :: Turn}

makeLenses ''Square

newtype Field = Field { fromField :: Map.Map (Int,Int) Square}


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

instance Show Square where
  show (Square King Later) = "g王"
  show (Square King First) = " 玉"
  show (Square pie Later) = "g"++ show pie
  show (Square pie First) = " " ++ show pie

instance Show Field where
  show (Field mp) = let showDan dan mp = (showColumnGrid [fromMaybe "　" $ fmap show $ (dan, i) `Map.lookup` mp | i <- [1..9]]) ++ " " ++ show (ChineseNumber dan) in
    showRowGrid $ (map (showDan `flip` mp) [1..9]) ++ ([showColumnGrid $ map (\n -> show n ++ " ") [1..9]])

showRowGrid :: [String] -> String
showRowGrid = foldl (\str strs -> str ++ "|" ++ strs) ""

showColumnGrid :: [String] -> String
showColumnGrid = foldl
  (\str strs -> strs ++ "\n" ++ replicate (length str) '―' ++ "\n" ++ str)
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
bothSymMap = foldlWithKey (\xs loc pie -> (insert (fifiPSymmetry loc) (pie & sqTurn .~ First) $ insert loc pie xs)) Map.empty

initialField :: Field
initialField = Field $ bothSymMap $ pawnList `Map.union` symmetric_part `Map.union` unsym_part
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
     ,((2,8), piece $ Rook Unpromoted)
     ]
