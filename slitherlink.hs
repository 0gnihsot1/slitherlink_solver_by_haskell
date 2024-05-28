-- スリザーリンクを解く
import Control.Monad

-- 問題
q00 :: Board
q00 = [[9, 9, 9, 9, 2,  9, 9, 2, 1, 9],
       [0, 3, 9, 9, 1,  9, 1, 9, 9, 0],
       [9, 9, 2, 3, 9,  9, 0, 9, 9, 2],
       [1, 9, 9, 9, 9,  9, 9, 3, 9, 9],
       [0, 9, 9, 3, 3,  9, 9, 0, 9, 9],
       
       [9, 9, 2, 9, 9,  0, 3, 9, 9, 0],
       [9, 9, 1, 9, 9,  9, 9, 9, 9, 2],
       [3, 9, 9, 0, 9,  9, 1, 2, 9, 9],
       [3, 9, 9, 2, 9,  2, 9, 9, 3, 3],
       [9, 3, 1, 9, 9,  1, 9, 9, 9, 9]]

-- 盤面
type Board = [[Int]]
-- ラインの盤面
type LineBoard = [[Int]]

-- 数字を取得する
getNum :: Board -> Int -> Int -> Int
getNum board x y = (board !! y) !! x

-- n 番目の要素を m に置き換える
subst :: [a] -> Int -> a -> [a]
subst [] _ _ = []
subst (x:xs) 0 m = m : xs
subst (x:xs) n m = x : subst xs (n - 1) m

-- 数字を書き込む
putNum :: LineBoard -> Int -> Int -> Int -> Board
putNum board x y n = subst board y $ subst (board !! y) x n

-- 盤面の横幅を取得する
getBoardWidth :: Board -> Int
getBoardWidth board = length (board !! 0)

-- 盤面の縦幅を取得する
getBoardHeight :: Board -> Int
getBoardHeight board = length board

-- 指定位置のLineBoardの位置を取得する
getLineBoardPosition :: LineBoard -> Int -> Int -> (Int, Int)
getLineBoardPosition lineBoard boardX boardY = (boardX * 2 + 1, boardY * 2 + 1)

-- 指定位置の周囲のライン数を取得する
getLineCnt :: Board -> Int -> Int -> Int
getLineCnt board x y = sum $ concatMap (f x') $ f y' board
  where x' = x * 2
        y' = y * 2
        f n xs = take 3 $ drop n xs

-- ラインの初期状態を取得する
getInitLineBoard :: Board -> LineBoard
getInitLineBoard board = getInitLineBoard3 board
  where width = (getBoardWidth board) * 2 + 1
        height = (getBoardHeight board) * 2 + 1
        getInitLineBoard2 [] = [0]
        getInitLineBoard2 (x:xs) = 0 : x : getInitLineBoard2 xs
        getInitLineBoard3 [] = [replicate width 0]
        getInitLineBoard3 (x:xs) = (replicate width 0) : getInitLineBoard2 x : getInitLineBoard3 xs

-- 展開する
expand :: [[a]] -> [a]
expand [] = []
expand (x:xs) = x ++ expand xs

-- すべての数字に対して線が引かれているか
isAllNumbersSatisfied :: Monad m => Board -> LineBoard -> m Bool
isAllNumbersSatisfied board lineBoard = iter board lineBoard makeIdx
  where makeIdx = [(x, y) | 
                    x <- [0..getBoardWidth board - 1],
                    y <- [0..getBoardHeight board - 1],
                    getNum board x y < 4]
        iter board lineBoard [] = return True
        iter board lineBoard ((x, y):idx) =
          if getNum board x y /= getLineCnt lineBoard x y then return False 
          else iter board lineBoard idx

-- 最初の数字の場所を取得する
getFirstNumPosition :: Board -> (Int, Int)
getFirstNumPosition board = (pos `mod` 10, pos `div` 10)
  where pos = length $ takeWhile checkEnable (expand board)
        checkEnable = \x -> x == 9 || x == 0

-- 解法
solver :: Board -> Board
solver board = solve board lineBoard (getFirstNumPosition board)
  where lineBoard = getInitLineBoard board
        solve board lineBoard position = board --iter board lineBoard makeIdx
        makeIdx = [(0,-1), (-1,0), (1,0), (0,1)]
        
