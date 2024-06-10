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
getNum board position@(x, y) = board !! y !! x

-- ラインを取得する(top)
getLineTop :: LineBoard -> Position -> Int
getLineTop lineBoard position@(x, y) = getNum lineBoard (x', y')
  where x' = x*3+1
        y' = y*3

-- ラインを取得する(right)
getLineRight :: LineBoard -> Position -> Int
getLineRight lineBoard position@(x, y) = getNum lineBoard (x', y')
  where x' = x*3+2
        y' = y*3+1

-- ラインを取得する(bottom)
getLineBottom :: LineBoard -> Position -> Int
getLineBottom lineBoard position@(x, y) = getNum lineBoard (x', y')
  where x' = x*3+1
        y' = y*3+2

-- ラインを取得する(left)
getLineLeft :: LineBoard -> Position -> Int
getLineLeft lineBoard position@(x, y) = getNum lineBoard (x', y')
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
putLine board position@(x, y) l = subst board y $ subst (board !! y) x l
  where subst [] _ _ = []
        subst (x:xs) 0 m = m : xs
        subst (x:xs) n m = x : subst xs (n - 1) m

-- ラインを引く
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
  where makeXs (x1:x2:x3:xs) =
          if null xs then [x1, x2, x3]
          else x1 : x2 : makeXs xs
        makeYs (x1:x2:x3:xs) =
          if null xs then [makeXs x1, makeXs x2, makeXs x3]
          else makeXs x1 : makeXs x2 : makeYs xs

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
getStartPoint :: LBoard -> Position -> (Position, Position)
getStartPoint lBoard position@(x, y) =
  if getNum lBoard topP == 1 then ((x'-1,y'-1), (x'+1,y'-1))
  else if getNum lBoard rightP == 1 then ((x'+1,y'-1), (x'+1,y'+1))
  else if getNum lBoard bottomP == 1 then ((x'+1,y'+1), (x'-1,x'+1))
  else ((x'-1,y'+1), (x'-1,y'-1))
  where x' = x*2+1
        y' = y*2+1
        topP = (x',y'-1)
        rightP = (x'+1,y')
        bottomP = (x',y'+1)
        leftP = (x'-1,y')

-- 線をなぞる
traceLine lBoard prev@(px, py) cur@(x, y) ds =
  if next `elem` ds then True
  else if next == (-1, -1) then False
  else traceLine lBoard cur next (ds ++ [cur])
  where top@(topX, topY) = (x, y-1)
        right@(rightX, rightY) = (x+1, y)
        bottom@(bottomX, bottomY) = (x, y+1)
        left@(leftX, leftY) = (x-1, y)
        width = getWidth lBoard
        height = getHeight lBoard
        next = if top /= prev && topY >= 0 && getNum lBoard top == line
                 then (topX, topY-1)
               else if right /= prev && rightX < width && getNum lBoard right == line
                 then (rightX+1, rightY)
               else if bottom /= prev && bottomX < height && getNum lBoard bottom == line
                 then (bottomX, bottomY+1)
               else if left /= prev && leftX >= 0 && getNum lBoard left == line
                 then (leftX-1, leftY)
               else (-1,-1)

-- 解法
solver :: Board -> [LineBoard]
solver board = iter initLineBoard positions
  where initLineBoard = getInitLineBoard board
        positions = [(x, y) | x <- [0..getWidth board-1], y <- [0..getHeight board-1]]
        iter lineBoard ((x, y) : ps) = do
          let p = (x, y)
              n = getNum board p
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
              (cur, next) = getStartPoint lBoard firstP
          guard (traceLine lBoard cur next [cur])
          return lBoard

-- 問題
q00 :: Board
q00 = [[1,3,3],
       [0,1,1]]

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
