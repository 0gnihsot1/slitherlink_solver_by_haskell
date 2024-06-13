-- スリザーリンクを解く
import Control.Monad

-- 盤面
type Board = [[Int]]
-- ラインボード
type LineBoard = [[Int]]
-- ラインボード(thin)
type LBoard = [[Int]]
-- ポジション
type Position = (Int, Int)

-- ライン無
notLine = 0
-- ライン有
line = 1
-- 空白
blank = 9

-- 盤面の横幅を取得する
getWidth :: Board -> Int
getWidth board = length (board !! 0)

-- 盤面の縦幅を取得する
getHeight :: Board -> Int
getHeight board = length board

-- ラインの初期状態を取得する
getInitLineBoard :: Board -> LineBoard
getInitLineBoard board = makeYs board
  where width = (getWidth board) * 3
        makeXs [] = []
        makeXs (x:xs) = 0 : x : 0 : makeXs xs
        makeYs [] = []
        makeYs (x:xs) = repWith0 : makeXs x : repWith0 : makeYs xs
        repWith0 = replicate width 0

-- 数字を取得する
getNum :: Board -> Position -> Int
getNum board (x, y) = board !! y !! x

-- ラインを取得する(top)
getLineTop :: LineBoard -> Position -> Int
getLineTop lineBoard (x, y) = getNum lineBoard (x', y')
  where x' = x*3+1
        y' = y*3

-- ラインを取得する(right)
getLineRight :: LineBoard -> Position -> Int
getLineRight lineBoard (x, y) = getNum lineBoard (x', y')
  where x' = x*3+2
        y' = y*3+1

-- ラインを取得する(bottom)
getLineBottom :: LineBoard -> Position -> Int
getLineBottom lineBoard (x, y) = getNum lineBoard (x', y')
  where x' = x*3+1
        y' = y*3+2

-- ラインを取得する(left)
getLineLeft :: LineBoard -> Position -> Int
getLineLeft lineBoard (x, y) = getNum lineBoard (x', y')
  where x' = x*3
        y' = y*3+1

-- ラインを取得する(topのbottom)
getLineBottomOfTop :: LineBoard -> Position -> Int
getLineBottomOfTop lineBoard (x, 0) = getLineTop lineBoard (x, 0)
getLineBottomOfTop lineBoard (x, y) = getLineBottom lineBoard (x, y-1)

-- ラインを取得する(leftのright)
getLineRightOfLeft :: LineBoard -> Position -> Int
getLineRightOfLeft lineBoard (0, y) = getLineLeft lineBoard (0, y)
getLineRightOfLeft lineBoard (x, y) = getLineRight lineBoard (x-1, y)

-- ラインを引く(個別)
putLine :: LineBoard -> Position -> Int -> LineBoard
putLine board (x, y) l = subst board y $ subst (board !! y) x l
  where subst [] _ _ = []
        subst (x:xs) 0 m = m : xs
        subst (x:xs) n m = x : subst xs (n - 1) m

-- ラインを引く(周囲)
putLines :: LineBoard -> Position -> Int -> Int -> Int -> Int -> LineBoard
putLines lineBoard (x, y) top right bottom left =
  putLineTop $ putLineRight $ putLineBottom $ putLineLeft lineBoard
  where putLineTop lineBoard = putLine lineBoard (x*3+1, y*3) top
        putLineRight lineBoard = putLine lineBoard (x*3+2, y*3+1) right
        putLineBottom lineBoard = putLine lineBoard (x*3+1, y*3+2) bottom
        putLineLeft lineBoard = putLine lineBoard (x*3, y*3+1) left

-- ラインボードを間引きする
thinLineBoard :: LineBoard -> LBoard
thinLineBoard lineBoard = makeYs lineBoard
  where makeXs (x1:x2:x3:xs)
          | null xs = [x1, x2, x3]
          | otherwise = x1 : x2 : makeXs xs
        makeYs (x1:x2:x3:xs)
          | null xs = [makeXs x1, makeXs x2, makeXs x3]
          | otherwise = makeXs x1 : makeXs x2 : makeYs xs

-- 最初の数字の場所を取得する
getFirstNumPosition :: Board -> Position
getFirstNumPosition board = (pos `mod` width, pos `div` height)
  where pos = length $ takeWhile checkEnable (expand board)
        checkEnable = \x -> x == blank || x == 0
        width = getWidth board
        height = getHeight board
        expand [] = []
        expand (x:xs) = x ++ expand xs

-- 線の始点を取得
getStartPoint :: LBoard -> Position -> (Position, Position, Position)
getStartPoint lBoard (x, y)
  | getNum lBoard topP == 1 = ((x'-1,y'-1), (x',y'-1), (x'+1,y'-1))
  | getNum lBoard rightP == 1 = ((x'+1,y'-1), (x'+1,y'), (x'+1,y'+1))
  | getNum lBoard bottomP == 1 = ((x'+1,y'+1), (x',y'+1), (x'-1,y'+1))
  | otherwise = ((x'-1,y'+1), (x'-1,y'), (x'-1,y'-1))
  where x' = x*2+1
        y' = y*2+1
        topP = (x',y'-1)
        rightP = (x'+1,y')
        bottomP = (x',y'+1)
        leftP = (x'-1,y')

-- 線をなぞる
traceLine :: LBoard -> Position -> Position -> Position -> [Position] -> Bool
traceLine lBoard prev@(px, py) lineP@(x, y) cur@(cx, cy) ds
  | next `elem` ds = getLinkCnt lBoard == length(ds ++ [cur])
  | next == (-1, -1) = False
  | otherwise = traceLine lBoard cur (getHalfPosition cur next) next (ds ++ [cur])
  where top@(tx, ty) = (cx, cy-1)
        right@(rx, ry) = (cx+1, cy)
        bottom@(bx, by) = (cx, cy+1)
        left@(lx, ly) = (cx-1, cy)
        width = getWidth lBoard
        height = getHeight lBoard
        next
          | top /= lineP && ty >= 0 && getNum lBoard top == line = (tx, ty-1)
          | right /= lineP && rx < width && getNum lBoard right == line = (rx+1, ry)
          | bottom /= lineP && by < height && getNum lBoard bottom == line = (bx, by+1)
          | left /= lineP && lx >= 0 && getNum lBoard left == line = (lx-1, ly)
          | otherwise = (-1, -1)

-- 点の中間を取得する
getHalfPosition :: Position -> Position -> Position
getHalfPosition (fx, fy) (tx, ty) = ((fx+tx) `div` 2, (fy+ty) `div` 2)

-- 展開する
expand :: [[Int]] -> [Int]
expand [] = []
expand (x:xs) = x ++ (expand xs)

-- 先の数を取得する
getLinkCnt :: LBoard -> Int
getLinkCnt lBoard = iter (expand lBoard) False
  where iter [] _ = 0
        iter (x:xs) True = x + (iter xs False)
        iter (x:xs) False = iter xs True

-- 解法
solver :: Board -> [LineBoard]
solver board = iter initLineBoard positions
  where initLineBoard = getInitLineBoard board
        positions = [(x, y) | x <- [0..getWidth board-1], y <- [0..getHeight board-1]]
        iter lineBoard (p@(x, y) : ps) = do
          let n = getNum board p
          top <- [notLine, line]
          right <- [notLine, line]
          bottom <- [notLine, line]
          left <- [notLine, line]
          let s = sum[top, right, bottom, left]
          guard (s < 4)
          guard (n == s || n == blank)
          guard (y == 0 || top == getLineBottomOfTop lineBoard p)
          guard (x == 0 || left == getLineRightOfLeft lineBoard p)
          iter (putLines lineBoard p top right bottom left) ps
        iter lineBoard [] = do
          let lBoard = thinLineBoard lineBoard
              firstP = getFirstNumPosition board
              (cur, lineP, next) = getStartPoint lBoard firstP
          guard (traceLine lBoard cur lineP next [cur])
          return lBoard

-- 問題
q00 :: Board
q00 = [[9,3,9],
       [0,9,1]]

q01 :: Board
q01 = [[9, 9, 9, 9, 2,  9, 9, 2, 1, 9],
       [0, 3, 9, 9, 1,  9, 1, 9, 9, 0],
       [9, 9, 2, 3, 9,  9, 0, 9, 9, 2],
       [1, 9, 9, 9, 9,  9, 9, 3, 9, 9],
       [0, 9, 9, 3, 3,  9, 9, 0, 9, 9],
       
       [9, 9, 2, 9, 9,  0, 3, 9, 9, 0],
       [9, 9, 1, 9, 9,  9, 9, 9, 9, 2],
       [3, 9, 9, 0, 9,  9, 1, 2, 9, 9],
       [3, 9, 9, 2, 9,  2, 9, 9, 3, 3],
       [9, 3, 1, 9, 9,  1, 9, 9, 9, 9]]
