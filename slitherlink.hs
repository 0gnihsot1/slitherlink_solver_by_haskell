-- スリザーリンクを解く

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

-- 指定位置の周囲のライン数を取得する
getLineCnt :: LineBoard -> Int -> Int -> Int
getLineCnt board x y = sum $ concatMap (f x') $ f y' board 
  where x' = x * 2
        y' = y * 2
        f n xs = take 3 $ drop n xs

-- ラインの初期状態を取得する
getInitLineBoard :: Int -> Int -> LineBoard
getInitLineBoard width height = replicate (height * 2 + 1) (replicate (width * 2 + 1) 0)

-- 展開する
expand [] = []
expand (x:xs) = x ++ expand xs

-- 引かれた線がひとつながりか
--isConnected

-- すべての数字に対して線が引かれているか
--isAllNumbersSatisfied

-- 最初の数字の場所を取得する
getFirstNumPosition :: Board -> (Int, Int)
getFirstNumPosition board = (pos `mod` 10, pos `div` 10)
  where pos = length $ takeWhile (\x -> x == 9 || x == 0) (expand board)

-- 解法
--solver :: Board -> [Board]
