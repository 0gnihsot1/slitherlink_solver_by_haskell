-- スリザーリンクを解く
import Control.Monad
import Data.Time

-- 盤面
type Board = [[Int]]
-- ラインボード
type LineBoard = [[Int]]
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
getWidth board = length (head board)

-- 盤面の縦幅を取得する
getHeight :: Board -> Int
getHeight = length

-- ラインの初期状態を取得する
getInitLineBoard :: Board -> LineBoard
getInitLineBoard board = makeYs board
  where width = getWidth board * 2 + 1
        makeXs [] = [notLine]
        makeXs (x:xs) = notLine : x : makeXs xs
        makeYs [] = [repWithNotLine]
        makeYs (x:xs) = repWithNotLine : makeXs x : makeYs xs
        repWithNotLine = replicate width notLine

-- 数字を取得する
getNum :: Board -> Position -> Int
getNum board (x, y) = board !! y !! x

-- ラインを取得する(top)
getLineTop :: LineBoard -> Position -> Int
getLineTop lBoard (x, y) = getNum lBoard (x*2+1, y*2)

-- ラインを取得する(left)
getLineLeft :: LineBoard -> Position -> Int
getLineLeft lBoard (x, y) = getNum lBoard (x*2, y*2+1)

-- ラインを引く(個別)
putLine :: LineBoard -> Position -> Int -> LineBoard
putLine lboard (x, y) l = subst lboard y $ subst (lboard !! y) x l
  where subst [] _ _ = []
        subst (x:xs) 0 m = m : xs
        subst (x:xs) n m = x : subst xs (n - 1) m

-- ラインを引く(周囲)
putLines :: LineBoard -> Position -> Int -> Int -> Int -> Int -> LineBoard
putLines lBoard (x, y) top right bottom left =
  putLineTop $ putLineRight $ putLineBottom $ putLineLeft lBoard
  where putLineTop lBoard = putLine lBoard (x*2+1, y*2) top
        putLineRight lBoard = putLine lBoard (x*2+2, y*2+1) right
        putLineBottom lBoard = putLine lBoard (x*2+1, y*2+2) bottom
        putLineLeft lBoard = putLine lBoard (x*2, y*2+1) left

-- 最初の数字の場所を取得する
getFirstNumPosition :: Board -> Position
getFirstNumPosition board = (pos `mod` width, pos `div` height)
  where pos = length $ takeWhile checkEnable (concat board)
        checkEnable x = x == blank || x == 0
        width = getWidth board
        height = getHeight board

-- 線の始点を取得
getStartPoint :: LineBoard -> Position -> (Position, Position, Position)
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
traceLine :: LineBoard -> Position -> Position -> Position -> Int -> Bool
traceLine lBoard lineP@(x, y) curP@(cx, cy) firstP cnt
  | nextP == (-1, -1) = False
  | nextP == firstP = getLinkCnt lBoard == cnt + 1
  | otherwise = traceLine lBoard (getHalfPosition curP nextP) nextP firstP (cnt+1)
  where top@(tx, ty) = (cx, cy-1)
        right@(rx, ry) = (cx+1, cy)
        bottom@(bx, by) = (cx, cy+1)
        left@(lx, ly) = (cx-1, cy)
        width = getWidth lBoard
        height = getHeight lBoard
        nextP
          | top /= lineP && ty >= 0 && getNum lBoard top == line = (tx, ty-1)
          | right /= lineP && rx < width && getNum lBoard right == line = (rx+1, ry)
          | bottom /= lineP && by < height && getNum lBoard bottom == line = (bx, by+1)
          | left /= lineP && lx >= 0 && getNum lBoard left == line = (lx-1, ly)
          | otherwise = (-1, -1)
        getHalfPosition (sx, sy) (ex, ey) = ((sx+ex) `div` 2, (sy+ey) `div` 2)

-- 線の数を取得する
getLinkCnt :: LineBoard -> Int
getLinkCnt lBoard = iter (concat lBoard) False
  where iter [] _ = 0
        iter (x:xs) True = x + iter xs False
        iter (x:xs) False = iter xs True

-- 点の周りの線の数を取得する
getLineCntArountDot :: LineBoard -> Position -> Int
getLineCntArountDot lBoard (x, y) = getTop + getRight + getBottom + getLeft
  where getTop
          | y == 0 = 0
          | otherwise = getNum lBoard (x, y-1)
        getRight
          | x == getWidth lBoard - 1 = 0
          | otherwise = getNum lBoard (x+1, y)
        getBottom
          | y == getHeight lBoard - 1 = 0
          | otherwise = getNum lBoard (x, y+1)
        getLeft
          | x == 0 = 0
          | otherwise = getNum lBoard (x-1, y)

-- 解法
solver :: Board -> [LineBoard]
solver board = iter (getInitLineBoard board) positions
  where positions = [(x, y) | x <- [0..getWidth board-1], y <- [0..getHeight board-1]]
        iter lBoard (p@(px, py) : ps) = do
          let n = getNum board p
          top <- [notLine, line]
          right <- [notLine, line]
          bottom <- [notLine, line]
          left <- [notLine, line]
          let s = top + right + bottom + left
          guard (n == s || n == blank)
          guard (py == 0 || top == getLineTop lBoard p)
          guard (px == 0 || left == getLineLeft lBoard p)
          let newBoard = putLines lBoard p top right bottom left
          guard (let val = getLineCntArountDot newBoard (px*2+2, py*2) in val < 3)
          guard (let val = getLineCntArountDot newBoard (px*2, py*2+2) in val < 3)
          guard (let val = getLineCntArountDot newBoard (px*2, py*2) in even val)
          iter newBoard ps
        iter lBoard [] = do
          let (curP, lineP, nextP) = getStartPoint lBoard (getFirstNumPosition board)
          guard (traceLine lBoard lineP nextP curP 1)
          return lBoard

main = do
  x <- getCurrentTime
  print x
  mapM_ (putStrLn.
    unwords.
    map show) (head $ solver r01)
  y <- getCurrentTime
  print y

-- 問題
q03 :: Board
q03 = [[9,9,3],
       [1,9,9],
       [3,9,0]]

q05 :: Board
q05 = [[9,3,9,9,0],
       [3,9,9,1,9],
       [9,9,9,9,9],
       [9,3,9,9,0],
       [2,9,9,3,9]]

r01 :: Board
r01 = [[9,9,9,9,9, 9,9,2,3,1],
       [9,9,9,9,0, 1,9,2,9,1],
       [0,1,1,9,2, 2,9,1,3,1],
       [2,9,2,9,9, 9,9,9,9,9],
       [3,1,1,9,0, 9,9,2,2,9],
       
       [9,9,9,9,9, 1,9,1,0,9],
       [9,9,1,1,9, 9,9,9,9,9],
       [0,9,1,2,9, 1,2,0,9,9],
       [9,9,9,9,9, 1,9,2,9,0],
       [9,9,9,9,9, 2,2,2,9,1]]
