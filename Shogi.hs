{-# LANGUAGE TemplateHaskell, MonadComprehensions, DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns#-}
{-# LANGUAGE Rank2Types #-}
module Shogi where

import Control.Monad.Trans(liftIO, lift, MonadIO)
import Control.Exception (try, IOException, SomeException, throwIO, catch)
import Data.Map as Map(Map, fromList, foldlWithKey, mapWithKey, union, mapKeys, insert, lookup, null, empty, filter, filterWithKey, delete, keys, (!))
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Functor(($>), (<$))
import Control.Lens ((%~), _1, _2, (.~), makeLenses, (&), both, (^.), use, (.=), (%=), at, Lens')
import Control.Monad (mplus, guard, forM_, MonadPlus, mzero, msum)
import Control.Monad.State (get, put, StateT(..), State, runStateT, evalStateT, execStateT)
import qualified Data.List as List(delete, nub, elem)
import Data.Monoid((<>))
import Text.Parsec as Parsec
import System.IO.Strict as Strict(readFile)
import Control.DeepSeq (deepseq)

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
-- パーサーみたいにエラーを文字列で出す

data Promotion = Promoted | Unpromoted deriving (Eq,Show,Read)
data Direction = DirMove | DirSet | ToDir (Int,Int) | IsPromotion Promotion | Top | DirectUp | Par | Subtraction | DirLeft | DirRight deriving (Eq,Show,Read)
data Move = Move { _movPiece :: Piece
                  ,_movDirs :: [Direction]
                  } deriving (Eq,Show,Read) -- 指し手

makeLenses ''Move

data Turn = First | Later deriving (Eq,Ord,Read,Show) -- 手番
-- isSenteban? 先手番かどうか

data Square = Square {_sqPiece::Piece, _sqTurn :: Turn} deriving (Eq,Read,Show)

makeLenses ''Square

data Field = Field { _fromField :: Map.Map (Int,Int) Square,
                     _caputured :: Map.Map Turn [Piece],
                     _isSenteban :: Bool } deriving (Eq,Show,Read) -- コモナド?

makeLenses ''Field


showSquare :: Square -> String
showSquare (Square King Later) = "g王"
showSquare (Square King First) = " 玉"
showSquare (Square pie Later) = "g"++ showPiece pie
showSquare (Square pie First) = " " ++ showPiece pie


showField :: Field -> String
showField Field {_fromField = mp, _caputured = captured} =
  let showDan dan mp = (showRowGrid [fromMaybe "　." $ (i, dan) `Map.lookup` mp | i <- [-1..10]])
    in let danScale = fromList [((0, n), show $ ChineseNumber n) | n <- [1..9]]-- 目盛り
      in let (cap::Map (Int, Int) String) = fromList $ [((-1, n), showPiece $ (captured Map.! First) !! n) | n <- [0..(length (captured Map.! First))-1], 0 <= n] <> [((10,n), showPiece $ (captured Map.! Later) !! n) | n <- [0..(length (captured Map.! Later))-1], 0 <= n] --持ち駒
        in let sujiScale = fromList [((n, 0), show n ++ "　") | n <- [1..9]]-- 目盛り
          in showColumnGrid $ (map (showDan `flip` (fmap showSquare mp `union` danScale `union` sujiScale `union` cap)) [0..9])

-- HTML！！


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
  show _ = error "Supported Chinese number is [1-9]."


symmetry :: Int -> Int
symmetry x = 10 - x

danSymmetry :: (Int, Int) -> (Int, Int)
danSymmetry = _1 %~ symmetry

sujiSymmetry :: (Int, Int) -> (Int, Int)
sujiSymmetry = _2 %~ symmetry

fifiPSymmetry :: (Int, Int) -> (Int, Int) -- (5,5) 点対称
fifiPSymmetry = danSymmetry . sujiSymmetry

--  lens であらゆるもののsymmetryをとれるようにすれば、盤反転の必要が無くなる？
--  true -> id
--  false -> symmetry のように返す
symAt :: (Int, Int) -> Lens' (Map.Map (Int, Int) Square) (Maybe Square)
symAt = at . fifiPSymmetry

varAt :: Bool -> (Int, Int) -> Lens' (Map.Map (Int, Int) Square) (Maybe Square)
varAt True = at
varAt False = symAt

relativeAt :: (Int, Int) -> Lens' Field (Maybe Square)
relativeAt (x, y) = undefined

danSymList :: [((Int, Int), Square)] -> [((Int, Int), Square)]
danSymList = foldl (\xs (loc, pie) -> (danSymmetry loc, pie):((loc, pie):xs)) []

bothSymMap :: Map.Map (Int, Int) Square -> Map.Map (Int, Int) Square
bothSymMap = foldlWithKey (\xs loc pie -> insert (fifiPSymmetry loc) (pie & sqTurn %~ turnChange) xs) Map.empty

reverseField :: Field -> Field
reverseField Field {_fromField=fie,_caputured= mochi,_isSenteban=sen} =
  Field {
    _fromField = foldlWithKey (\xs loc pie -> insert (fifiPSymmetry loc) (pie & sqTurn %~ turnChange) xs) Map.empty fie
    ,_caputured  = fromList [(First, mochi Map.! Later), (Later, mochi Map.! First)]
    ,_isSenteban=sen
  }

initialField :: Field
initialField = let later_field = pawnList `Map.union` symmetric_part `Map.union` unsym_part
                   in Field {
                     _fromField = (later_field `union` bothSymMap later_field),
                     _caputured = fromList [(First, []), (Later, [])],
                     _isSenteban = True}
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

initialQField :: [Field] -- 量子将棋の初期盤面
initialQField = undefined


moveMap :: Ord i => i -> i -> Map.Map i a -> Map.Map i a
moveMap i j mp = let a = mp ! i in insert j a $ Map.delete i mp

-- symmMove

moveMapZero :: (Ord i, MonadPlus m) => i -> i -> Map.Map i a -> m (Map.Map i a)
moveMapZero i j mp = let x = Map.lookup i mp in
  case x of
    Just a -> return $ insert j a (Map.delete i mp)
    Nothing -> mzero

moveMapZeroProm :: MonadPlus m => (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) Square -> m (Map.Map (Int, Int) Square)
moveMapZeroProm i@(ix,iy) j@(jx,jy) mp = let x = Map.lookup i mp in
  case x of
    Just a -> do
      guard $ iy <= 3 || jy <= 3
      return $ insert j (a & sqPiece %~ promotion) (Map.delete i mp)
    Nothing -> mzero

unmovableZero :: MonadPlus m => Piece -> (Int, Int) -> m () -- 不成のルール
unmovableZero pie xy = do
  guard (1 /= xy^._2 || (pie /= Pawn Unpromoted && pie /= Lance Unpromoted))
  guard (2 < xy^._2 || (pie /= Knight Unpromoted))

applyPiece :: PieceM (Int, Int) -> PieceM (Either (Piece, (Int, Int)) (Int, Int))
applyPiece f = do
  original_xy <- use _1
  xy <- f
  _1 .= xy
  original_field <- use $ _2 . fromField

  case Map.lookup xy original_field of
     Just (Square pie First) -> mzero
     Just (Square pie Later) -> do
       field <- moveMapZeroProm original_xy xy original_field -- 成る場合
                `mplus` moveMapZero original_xy xy original_field
       _2 %= (\(Field {_caputured= cap, _isSenteban=sen}) -> Field {_fromField = field, _caputured = Map.insert First ((Unpromoted <$ pie):(cap Map.! First)) cap, _isSenteban=sen})

       let pie = ((field ! xy) ^. sqPiece)
       guard (1 /= xy^._2 || (pie /= Pawn Unpromoted && pie /= Lance Unpromoted))
       guard (2 <= xy^._2 || (pie /= Knight Unpromoted))
       return $ Left (pie, xy)
     Nothing -> do
       field <- moveMapZeroProm original_xy xy original_field
                `mplus` moveMapZero original_xy xy original_field
       _2 %= (\(Field {_caputured=cap, _isSenteban=sen}) -> Field {_fromField=field,_caputured= cap, _isSenteban=sen})

       let pie = ((field ! xy) ^. sqPiece)
       guard (1 /= xy^._2 || (pie /= Pawn Unpromoted && pie /= Lance Unpromoted))
       guard (2 <= xy^._2 || (pie /= Knight Unpromoted))
       return $ Right xy

right1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
right1 = applyPiece [(x-1, y) | (x, y) <- use _1, 0 < x-1]

left1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
left1 = applyPiece [(x+1, y) | (x,y) <- use _1, x+1 <= 9]

up1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
up1 = applyPiece [(x, y-1) | (x, y) <- use _1, 0 < y-1]

down1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
down1 = applyPiece [(x, y+1) | (x, y) <- use _1, y+1 <= 9 ]

vector1 :: (Int, Int) -> PieceM (Either (Piece, (Int, Int)) (Int, Int))
vector1 (n, m) = applyPiece $ [(x+n, y+m) | (x,y) <- use _1, x+n <= 9, y+m <= 9, 0 < x+n, 0 < y+m]

corner1f :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
corner1f = vector1 (1, -1) `mplus` vector1 (-1, -1)

corner1b :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
corner1b = vector1 (1, 1) `mplus` vector1 (-1, 1)

moveAll :: PieceM (Either (Piece, (Int, Int)) (Int, Int)) -> PieceM (Int, Int)
-- Eitherは、駒を取ったかどうかを表す
moveAll move1 = do
  loc2 <- move1 -- move1に歩を指定すると、`moveAll 歩`は香車の動きになる
  case loc2 of
    Left (pie, xy) -> do
      _1 .= xy
      return (xy::(Int, Int))
    Right xy -> do -- 空白のマス目の場合再帰できる
      _1 .= xy
      (moveAll move1) `mplus` use _1 -- もう一度動くか、止まるか  選ぶ

height1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
height1 = up1 `mplus` down1

width1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
width1 = right1 `mplus` left1

corner1 :: PieceM (Either (Piece, (Int, Int)) (Int, Int))
corner1 = corner1f `mplus` corner1b

cornerAll :: PieceM (Int, Int)
cornerAll =
  moveAll (vector1 (1, 1))
  `mplus` moveAll (vector1 (1, -1))
  `mplus` moveAll (vector1 (-1, 1))
  `mplus` moveAll (vector1 (-1, -1))

widthAll :: PieceM (Int, Int)
widthAll = moveAll right1 `mplus` moveAll left1

upAll :: PieceM (Int, Int)
upAll = moveAll up1

heightAll :: PieceM (Int, Int)
heightAll = upAll `mplus` moveAll down1


type PieceM a = StateT ((Int, Int), Field) [] a -- 駒モナド
-- ロールバック

eitherPoint :: Either (Piece, (Int, Int)) (Int, Int) -> (Int, Int)
eitherPoint = either (^. _2) id



movable :: Piece -> PieceM (Int, Int)
movable King = do
  loc1 <- use _1
  loc2 <- (fmap eitherPoint width1) `mplus` (fmap eitherPoint height1) `mplus` (fmap eitherPoint corner1)
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
  (loc2::(Int,Int)) <- (fmap eitherPoint width1) `mplus` (fmap eitherPoint height1) `mplus` (fmap eitherPoint corner1f)
  guard (loc1 /= loc2)
  return loc2
movable (Silver Unpromoted) = do
  loc1 <- use _1
  loc2 <- fmap eitherPoint up1 `mplus` fmap eitherPoint corner1
  guard (loc1 /= loc2)
  return loc2
movable (Silver Promoted) = movable Gold
movable (Knight Unpromoted) = do
  loc <- fmap eitherPoint $ vector1 (1, -2) `mplus` vector1 (-1, -2)
  return loc
movable (Knight Promoted) = movable Gold
movable (Lance Unpromoted) = do
  loc1 <- use _1
  loc2 <- upAll
  --loc2 <- moveAll (movable (Pawn Unpromoted))
  guard (loc1 /= loc2)
  return loc2
movable (Lance Promoted) = movable Gold
movable (Pawn Unpromoted) = fmap eitherPoint up1
movable (Pawn Promoted) = movable Gold



-- 駒の動きをデータ型で表す
data AtomicKoma = KomaUp
                  | KomaDown
                  | KomaRight
                  | KomaLeft
                  deriving (Eq, Show, Read)
                  -- 飛車 = (KomaUp+ | KomaDown+ | KomaRight+ | KomaLeft+)

data KomaAlgebra a = Atom a
                  | Or (KomaAlgebra a) (KomaAlgebra a)  -- 選択
                  | And (KomaAlgebra a) (KomaAlgebra a) -- 連結
                  | JumpAnd (KomaAlgebra a) (KomaAlgebra a) -- ジャンプ連結
                  | Power (KomaAlgebra a) -- 1回以上の繰り返し

type KomaUgoki = KomaAlgebra AtomicKoma


interpretAtomicKoma :: AtomicKoma -> PieceM (Either (Piece, (Int, Int)) (Int, Int))
interpretAtomicKoma KomaUp = up1
interpretAtomicKoma KomaDown = down1
interpretAtomicKoma KomaRight = right1
interpretAtomicKoma KomaLeft = left1

interpretKomaAlgebra :: KomaAlgebra (PieceM (Either (Piece, (Int, Int)) (Int, Int))) -> PieceM (Either (Piece, (Int, Int)) (Int, Int))
interpretKomaAlgebra (Atom a) = a
interpretKomaAlgebra (Or a b) = interpretKomaAlgebra a `mplus` interpretKomaAlgebra b
interpretKomaAlgebra (And a b) = interpretKomaAlgebra a >> interpretKomaAlgebra b
interpretKomaAlgebra (JumpAnd a b) = interpretKomaAlgebra a >> interpretKomaAlgebra b
interpretKomaAlgebra (Power a) = interpretKomaAlgebra a
                              >> (undefined
                                  `mplus` interpretKomaAlgebra (Power a))

promotion :: Piece -> Piece
promotion = (Promoted <$)

--filterM :: (a -> m Bool) -> [a] -> m [a]
--mapM (a -> m b) -> [a] -> m [b]

--filterM :: (a -> [Bool]) -> [a] -> [[a]]
--mapM (a -> [b]) -> [a] -> [[b]]


-- moveFrom

setMapZero :: MonadPlus m => (Int, Int) -> Piece -> Map.Map (Int, Int) Square -> m (Map.Map (Int, Int) Square)
setMapZero i@(ix, iy) pie mp = do
  let x = Map.lookup i mp
  case x of
    Just _ -> mzero
    Nothing -> do
      unmovableZero pie i
      return $ insert i (Square pie First) mp

-- 二歩
ruleOfTwoPone :: MonadPlus m => Piece -> (Int, Int) -> Map.Map (Int, Int) Square -> m ()
ruleOfTwoPone pie i@(ix,iy) mp =
  guard $ (||) (pie /= Pawn Unpromoted) $ foldl (&&) True $ [(Map.lookup (ix, y) mp) /= Just (Square (Pawn Unpromoted) First) | y <- [1..9]]

setCaptured :: MonadPlus m => Piece -> (Int, Int) -> Field -> m Field
setCaptured pie i@(ix, iy) (Field {_fromField=fie,_caputured= cap, _isSenteban=sen}) = do
  let my_cap = cap ! First
  guard $ pie `List.elem` my_cap
  fie2 <- setMapZero i pie fie
  return $ Field {_fromField = fie2 ,  _caputured = insert First (List.delete pie $ my_cap) $ cap, _isSenteban=sen}

settableZone :: Piece -> Field -> [(Int, Int)]
settableZone pie fie@(Field {_fromField=fi,_caputured= cap}) = do
  k <- [ (x, y) | x <- [1..9], y <-[1..9] ]
  unmovableZero pie k
  case k `Map.lookup` fi of
       Just a -> mzero
       Nothing -> return k

moveOrSet :: Move -> Field -> [Field] -- 動かす or 打つ
moveOrSet mv@(Move pie dirs) field = move mv field `mplus` set mv field where
  set mv@(Move pie dirs) field = do
    guard $ not $ DirMove `List.elem` dirs
    settable <- settableZone pie field
    ruleOfTwoPone pie settable $ field ^. fromField
    forM_ dirs $ \d -> case d of
                            ToDir xy -> guard $ xy == settable
                            _ -> return ()
    map reverseField $ setCaptured pie settable field


moveFrom :: (Int, Int) -> [Direction] -> Field -> [Field]
moveFrom i@(ix, iy) dirs field@(Field {_fromField=mp,_caputured= cap}) =
  let (ex::[((Int,Int),Field)]) = Prelude.filter
                                    (\(l,a) -> directions ((mp ! i)^. sqPiece) i (l, a) dirs) $ execStateT (movable ((mp ! i)^. sqPiece)) (i, field)
                                    in map reverseField $ map (^. _2) ex

-- TODO: 持ち駒を打つ mplus
move :: Move -> Field -> [Field]
move (Move pie dirs) field =
  let (pies::[(Int,Int)]) = keys $ Map.filter (== (Square pie First)) $ (^.fromField) field in
  let (ex::[((Int,Int),Field)]) = concatMap
                                  (\k -> Prelude.filter
                                    (\(l,a) -> directions pie k (l, a) dirs) $ execStateT (movable pie) (k, field)) pies
                                    in do
                                     guard $ not $ DirSet `List.elem` dirs
                                     map reverseField $ map (^. _2) ex

direction :: Piece -> (Int, Int) -> ((Int, Int), Field) -> Direction -> Bool
direction o_pie original_xy (l, a) (IsPromotion is_prom) = (Just is_prom == (fmap (\x -> isPromoted (x^.sqPiece)) $ Map.lookup l $ (a^. fromField))) && (Unpromoted == isPromoted o_pie)
direction o_pie original_xy (l, a) (ToDir loc) = loc == l
direction o_pie original_xy@(ox,oy) (l@(newx,newy), a) Subtraction = newy > oy
direction o_pie original_xy@(ox,oy) (l@(newx,newy), a) Par = newy == oy && abs (newx - ox) == 1
direction o_pie original_xy@(ox,oy) (l@(newx,newy), a) Top = newy < oy
direction o_pie original_xy@(ox,oy) (l@(newx,newy), a) DirRight = newx > ox
direction o_pie original_xy@(ox,oy) (l@(newx,newy), a) DirLeft = ox > newx
direction o_pie original_xy@(ox,oy) (l@(newx,newy), a) DirectUp = ox == newx && newy < oy

directions :: Piece -> (Int, Int) -> ((Int, Int), Field) -> [Direction] -> Bool
directions o_pie original_xy fields dirs = foldl (&&) True $ map (direction o_pie original_xy fields) dirs

isPromoted :: Piece -> Promotion
isPromoted King = Unpromoted
isPromoted Gold = Unpromoted
isPromoted (Silver x) = x
isPromoted (Knight x) = x
isPromoted (Lance x) = x
isPromoted (Pawn x) = x
isPromoted (Rook x) = x
isPromoted (Bishop x) = x

check :: PieceM Bool
check = undefined

checkmate :: PieceM (Int, Int)
checkmate = undefined

-- 毎回盤をひっくり返すことで手番を表せる

turnChange :: Turn -> Turn
turnChange First = Later
turnChange Later = First

{-shogiTest :: String
shogiTest = (concat $ map showField $ Shogi.move (Shogi.Move (Shogi.King) (4, 8) []) $ Shogi.initialField)
        <> "\n" <> (concat $ map showField $ map (\((x1,y1),fie) -> fie) $ execStateT (Shogi.movable (Shogi.Lance Shogi.Unpromoted)) ((2, 7), initialField))
        <> "\n" <> (concat $ map (showField . (^._2)) $ execStateT (Shogi.movable King) ((5, 9), head $ move (Shogi.Move Gold (5, 8) []) $ head $ move (Shogi.Move Gold (5, 8) []) $ initialField))
-}

chineseNumParser :: (Monad m) => ParsecT String u m Int
chineseNumParser = do
  c <- msum $ map char "一ニ二三四五六七八九"
  case c of
    '一' -> return $ 1
    'ニ' -> return $ 2
    '二' -> return $ 2
    '三' -> return $ 3
    '四' -> return $ 4
    '五' -> return $ 5
    '六' -> return $ 6
    '七' -> return $ 7
    '八' -> return $ 8
    '九' -> return $ 9
    c -> fail $ c:" is not chineseNum(漢数字.)"

pieceParser :: (Monad m) => ParsecT String u m Shogi.Piece
pieceParser = do
  nari <- Shogi.Promoted <$ (char '成') <|> return Shogi.Unpromoted
  str <- msum $ map (Parsec.try . string) ["歩", "香", "香車", "桂", "桂馬", "銀", "金", "玉", "王", "飛", "飛車", "角", "竜", "馬", "と金", "と"]

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
                                "竜" -> Shogi.Rook Shogi.Promoted
                                "馬" -> Shogi.Bishop Shogi.Promoted
                                "と金" -> Shogi.Pawn Shogi.Promoted
                                "と" -> Shogi.Pawn Shogi.Promoted
                                _ -> error "駒ではない。"

  return $ case nari of
    Promoted -> Promoted <$ unpromoteds
    Unpromoted -> unpromoteds

dirParser :: (Monad m) => ParsecT String u m Direction
dirParser = (Subtraction <$ string "引")
  <|> (Par <$ string "寄")
  <|> (Top <$ string "上")
  <|> (DirRight <$ string "右")
  <|> (DirLeft <$ string "左")
  <|> (IsPromotion Shogi.Unpromoted <$ (string "不成"))
  <|> (IsPromotion Shogi.Promoted <$ (char '成'))
  <|> (DirectUp <$ (char '直'))
  <|> (DirSet <$ (char '打'))
  <|> (DirMove <$ (char '動'))

moveParser :: (Monad m) => ParsecT String u m (Shogi.Move)
moveParser = do
  maySym <- (Shogi.fifiPSymmetry <$ char '△') <|> (id <$ return "")
  n <- (read <$> (msum $ map (fmap (\x->[x]) . char) "123456789")) <|> chineseNumParser
  m <- (read <$> (msum $ map (fmap (\x->[x]) . char) "123456789")) <|> chineseNumParser
  piece <- pieceParser
  dirs <- many dirParser
  return $ Shogi.Move piece $ (ToDir $ maySym (n,m)):dirs

-- 成るのと成らないのを非決定的に行う→DONE
-- TODO: 王手判定
-- TODO: 持ち駒を打つ DONE
-- △で反転→DONE
-- 銀成と成銀の区別ある? →DONE
-- 成駒の動きがおかしい
-- html/cssで非決定盤面を表現
-- table
--   表現するために盤を潰す操作を定義する
--   [Field] -> QField
--   やっぱいいか？

shogiParser :: (MonadIO m) => ParsecT String u m String
shogiParser = Parsec.try $ do
  _ <- string "shogi" <|> string "sh" <|> string "将棋"
  skipMany space

  str <- (do
    mayReverse <- (Shogi.reverseField <$ string "▲") <|> (id <$ return "")
    mv <- Shogi.moveParser
    !old_field_str <- lift $ liftIO $ Prelude.readFile "shogi.txt" `catch` (\(e::IOException) -> return $ show [Shogi.initialField])
    let old_field = read old_field_str :: [Shogi.Field]
    let newField = List.nub $ concatMap (Shogi.moveOrSet mv) old_field

    lift $ liftIO $ old_field_str `deepseq` Prelude.writeFile "shogi.txt" $ show (newField :: [Shogi.Field])
    return $ concat $ map ((++"\n").Shogi.showField) $ map mayReverse $ newField
    ) <|> (do
      _ <- string "init"
      lift $ liftIO $ Prelude.writeFile "shogi.txt" $ show [Shogi.initialField]
      return $ Shogi.showField Shogi.initialField
      ) <|> (do
      _ <- string "display"
      skipMany space
      !(old_field::[Shogi.Field]) <-  (do
        _ <- string "reverse"
        fmap (map Shogi.reverseField) $ fmap read $ lift $ liftIO $ Strict.readFile "shogi.txt" `catch` (\(e::IOException) -> return $ show [Shogi.initialField])) <|> (fmap read $ lift $ liftIO $ Strict.readFile "shogi.txt" `catch` (\(e::IOException) -> return $ show [Shogi.initialField]))

      return $ concatMap ((++"\n").Shogi.showField) $ (old_field :: [Shogi.Field])
      )
  return str

-- html version


-- \f -> foldl (&&) True . map f

-- data UndoTree a  zipper
-- 八方桂、獅子、量子将棋
