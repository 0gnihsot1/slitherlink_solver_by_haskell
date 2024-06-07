-- スリザーリンクを解く
import Control.Monad

-- 盤面
type Board = [[Int]]
-- ラインの盤面
type LineBoard = [[Int]]
-- ポジション
type Position = (Int, Int)

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
        height = (getHeight board) * 3
        makeXs [] = []
        makeXs (x:xs) = 0 : x : 0 : makeXs xs
        makeYs [] = []
        makeYs (x:xs) = (replicate width 0) : makeXs x : (replicate width 0) : makeYs xs

-- ラインを引く
putLine :: LineBoard -> Position -> Int -> LineBoard
putLine board (x, y) l = subst board y $ subst (board !! y) x l
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

-- ラインを取得する(rightのleft)
getLineLeftOfRight :: Board -> LineBoard -> Position -> Int
getLineLeftOfRight board lineBoard (x, y) =
  if getWidth board - 1 == x then getLineRight lineBoard (x, y)
  else getLineLeft lineBoard (x+1, y)

-- ラインを取得する(bottomのtop)
getLineTopOfBottom :: Board -> LineBoard -> Position -> Int
getLineTopOfBottom board lineBoard (x, y) =
  if getHeight board - 1 == y then getLineBottom lineBoard (x, y)
  else getLineTop lineBoard (x, y+1)

-- ラインを取得する(leftのright)
getLineRightOfLeft :: LineBoard -> Position -> Int
getLineRightOfLeft lineBoard (0, y) = getLineLeft lineBoard (0, y)
getLineRightOfLeft lineBoard (x, y) = getLineRight lineBoard (x-1, y)

-- 解法
solver :: Board -> [LineBoard]
solver board = iter initLineBoard makeIdx
  where initLineBoard = getInitLineBoard board
        makeIdx = [(x, y) | x <- [0..getWidth board-1], y <- [0..getHeight board-1]]
        iter lineBoard [] = return lineBoard
        iter lineBoard (idx : idxs) = do
          top <- [0, 1]
          right <- [0, 1]
          bottom <- [0, 1]
          left <- [0, 1]
          let s = sum[top, right, bottom, left]
          guard (s < 4)
          guard (s == getNum board idx || getNum board idx == blank)
          guard (top == getLineBottomOfTop lineBoard idx)
          guard (right == getLineLeftOfRight board lineBoard idx)
          guard (bottom == getLineTopOfBottom board lineBoard idx)
          guard (left == getLineRightOfLeft lineBoard idx)
          iter (putLines lineBoard idx top right bottom left) idxs

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
