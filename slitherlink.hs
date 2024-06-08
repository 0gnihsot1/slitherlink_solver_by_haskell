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

-- ラインを引く
putLines :: LineBoard -> Position -> Int -> Int -> Int -> Int -> LineBoard
putLines lineBoard (x, y) top right bottom left =
  putLineTop $ putLineRight $ putLineBottom $ putLineLeft lineBoard
  where putLineTop lineBoard = putLine lineBoard (x*3+1, y*3) top
        putLineRight lineBoard = putLine lineBoard (x*3+2, y*3+1) right
        putLineBottom lineBoard = putLine lineBoard (x*3+1, y*3+2) bottom
        putLineLeft lineBoard = putLine lineBoard (x*3, y*3+1) left

-- 解法
solver :: Board -> [LineBoard]
solver board = iter initLineBoard positions
  where initLineBoard = getInitLineBoard board
        positions = [(x, y) | x <- [0..getWidth board-1], y <- [0..getHeight board-1]]
        iter lineBoard [] = return lineBoard
        iter lineBoard ((x, y) : ps) = do
          top <- [0, 1]
          right <- [0, 1]
          bottom <- [0, 1]
          left <- [0, 1]
          let p = (x, y)
              s = sum[top, right, bottom, left]
              n = getNum board p
          guard (s < 4)
          guard (n == s || n == blank)
          guard (x == 0 || top == getLineBottomOfTop lineBoard p)
          guard (y == 0 || left == getLineRightOfLeft lineBoard p)
          iter (putLines lineBoard p top right bottom left) ps

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
