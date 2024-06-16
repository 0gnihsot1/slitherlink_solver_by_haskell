-- スリザーリンクを解く
import Control.Monad

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
getWidth board = length (board !! 0)

-- 盤面の縦幅を取得する
getHeight :: Board -> Int
getHeight board = length board

-- ラインの初期状態を取得する
getInitLineBoard :: Board -> LineBoard
getInitLineBoard board = makeYs board
  where width = (getWidth board) * 2 + 1
        makeXs [] = [notLine]
        makeXs (x:xs) = notLine : x : makeXs xs
        makeYs [] = [repWith0]
        makeYs (x:xs) = repWith0 : makeXs x : makeYs xs
        repWith0 = replicate width notLine

-- 数字を取得する
getNum :: Board -> Position -> Int
getNum board (x, y) = board !! y !! x

-- ラインを引く
putLine :: LineBoard -> Position -> LineBoard
putLine lboard (x, y) = subst lboard y $ subst (lboard !! y) x line
  where subst [] _ _ = []
        subst (x:xs) 0 m = m : xs
        subst (x:xs) n m = x : subst xs (n - 1) m

-- 最初の数字の場所を取得する
getFirstNumPosition :: Board -> Position
getFirstNumPosition board = (pos `mod` width, pos `div` height)
  where pos = length $ takeWhile checkEnable (concat board)
        checkEnable = \x -> x == blank || x == 0
        width = getWidth board
        height = getHeight board

-- 解法
solver :: Board -> [LineBoard]
solver board = do
  let lBoard = getInitLineBoard board
      firstP@(fx, fy) = getFirstNumPosition board
      width = getWidth lBoard
      height = getHeight lBoard
  lPositions@(fromP, lineP@(x,y), toP) <- [((fx*2,fy*2), (fx*2+1,fy*2), (fx*2+2,fy*2)),
                                           ((fx*2+1,fy*2), (fx*2+1,fy*2+1), (fx*2+1,fy*2+2)),
                                           ((fx*2+2,fy*2+2), (fx*2+1,fy*2+2), (fx*2,fy*2+2)),
                                           ((fx*2,fy*2+2), (fx*2,fy*2+1), (fx*2,fy*2))]
  guard (x >= 0 && x < width)
  guard (y >= 0 && y < height)
  drawLine (putLine lBoard lineP) lPositions [lineP]

-- 可能な限り線を引き続ける
drawLine lBoard p@((fx, fy), (lx, ly), (tx, ty)) ps = do
  let width = getWidth lBoard
      height = getHeight lBoard
  lPositions@((fromP, lineP@(x,y), toP)) <- [((tx,ty), (tx+1,ty), (tx+2,ty)),
                                             ((tx,ty), (tx,ty+1), (tx,ty+2)),
                                             ((tx,ty), (tx-1,ty), (tx-2,ty)),
                                             ((tx,ty), (tx,ty-1), (tx,ty-2))]
  guard (x >= 0 && x < width)
  guard (y >= 0 && y < height)
  return lBoard

main = do
  print $ solver q00

-- 問題
q00 :: Board
q00 = [[9,3,9],
       [0,9,1]]

q03 :: Board
q03 = [[9,9,3],
       [1,9,9],
       [3,9,0]]

q03l :: LineBoard
q03l = [[0,1,0,1,0,1,0],
        [1,9,0,9,0,3,1],
        [0,0,0,0,0,1,0],
        [1,1,0,9,1,9,0],
        [0,0,0,1,0,0,0],
        [1,3,1,9,0,0,0],
        [0,1,0,0,0,0,0]]

q05 :: Board
q05 = [[9,3,9,9,0],
       [3,9,9,1,9],
       [9,9,9,9,9],
       [9,3,9,9,0],
       [2,9,9,3,9]]

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
