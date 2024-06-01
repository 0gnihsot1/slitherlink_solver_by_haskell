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
-- 枠をはみ出した場合はエラーとなる
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

-- ラインを引く
putLine :: LineBoard -> Int -> Int -> LineBoard
putLine board x y = putNum board x y 1

-- 盤面の横幅を取得する
getBoardWidth :: Board -> Int
getBoardWidth board = length (board !! 0)

-- 盤面の縦幅を取得する
getBoardHeight :: Board -> Int
getBoardHeight board = length board

-- 指定位置の周囲のライン数を取得する
getLineCnt :: LineBoard -> Int -> Int -> Int
getLineCnt board x y = sum $ map f makeIdx
  where makeIdx = [(0,-1), (1,0), (0,1), (-1,0)]
        f (x', y') = getNum board (x + x') (y + y')

-- ラインの初期状態を取得する
getInitLineBoard :: Board -> LineBoard
getInitLineBoard board = makeYs board
  where width = (getBoardWidth board) * 2 + 1
        height = (getBoardHeight board) * 2 + 1
        makeXs [] = [0]
        makeXs (x:xs) = 0 : x : makeXs xs
        makeYs [] = [replicate width 0]
        makeYs (x:xs) = (replicate width 0) : makeXs x : makeYs xs

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
getFirstNumPosition board = (pos `mod` width, pos `div` height)
  where pos = length $ takeWhile checkEnable (expand board)
        checkEnable = \x -> x == 9 || x == 0
        width = getBoardWidth board
        height = getBoardHeight board

-- 引いたラインの両脇の整合性をチェックする
checkLineIntegrity :: LineBoard -> Int -> Int -> Bool
checkLineIntegrity lineBoard x y = 
  if odd y then check (x-1) y && check (x+1) y
  else check x (y-1) && check x (y+1)
  where check (-1) _ = True
        check _ (-1) = True
        check x' y' = getLineCnt lineBoard x' y' <= getNum lineBoard x' y'

-- 解法
--solver :: Board -> LineBoard
--TODO 
solver board = solve lineBoard (getFirstNumPosition lineBoard)
  where lineBoard = getInitLineBoard board
        solve board (x, y) = iter lineBoard x y makeIdx
        makeIdx = [(0,-1), (1,0), (0,1), (-1,0)]
        iter lineBoard x y ((x',y'):idx) = do
          let targetX = x + x'
              targetY = y + y'
          guard (lineBoard !! targetY !! targetX == 0)
          guard (checkLineIntegrity lineBoard targetX targetY)
          return lineBoard
        
