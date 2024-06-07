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
-- ライン
line = 1

-- 数字を取得する
-- 枠をはみ出した場合はエラーとなる
getNum :: Board -> Position -> Int
getNum board (x, y) = board !! y !! x

-- ラインを引く
putLine :: LineBoard -> Position -> LineBoard
putLine board (x, y) = subst board y $ subst (board !! y) x line
  where subst [] _ _ = []
        subst (x:xs) 0 m = m : xs
        subst (x:xs) n m = x : subst xs (n - 1) m

-- 盤面の横幅を取得する
getWidth :: Board -> Int
getWidth board = length (board !! 0)

-- 盤面の縦幅を取得する
getHeight :: Board -> Int
getHeight board = length board

-- 指定位置の周囲のライン数を取得する
getLineCnt :: LineBoard -> Position -> Int
getLineCnt board (x, y) = sum $ map f makeIdx
  where makeIdx = [(0,-1), (1,0), (0,1), (-1,0)]
        f (x', y') = getNum board ((x + x'), (y + y'))

-- ラインの初期状態を取得する
getInitLineBoard :: Board -> LineBoard
getInitLineBoard board = makeYs board
  where width = (getWidth board) * 2 + 1
        height = (getHeight board) * 2 + 1
        makeXs [] = [0]
        makeXs (x:xs) = 0 : x : makeXs xs
        makeYs [] = [replicate width 0]
        makeYs (x:xs) = (replicate width 0) : makeXs x : makeYs xs

-- すべての数字に対して線が引かれているか
checkAllNumSatisfied :: Board -> LineBoard -> Bool
checkAllNumSatisfied board lineBoard = and $ map f makeIdx
  where makeIdx = [(x, y) | 
                    x <- [0..getWidth board - 1],
                    y <- [0..getHeight board - 1],
                    getNum board (x, y) < 4]
        f (x, y) = getLineCnt lineBoard (x*2+1, y*2+1) == getNum board (x, y)

-- 最初の数字の場所を取得する
getFirstNumPosition :: Board -> Position
getFirstNumPosition board = (pos `mod` width, pos `div` height)
  where pos = length $ takeWhile checkEnable (expand board)
        checkEnable = \x -> x == blank || x == 0
        width = getWidth board
        height = getHeight board
        expand [] = []
        expand (x:xs) = x ++ expand xs

-- 引いたラインの両脇の整合性をチェックする
--checkLineIntegrity :: LineBoard -> Position -> Bool
checkLineIntegrity lineBoard (x, y) = 
  if odd y then check ((x-1), y) && check ((x+1), y)
  else check (x, (y-1)) && check (x, (y+1))
  where check ((-1), _) = True
        check (_, (-1)) = True
        check (x', y') = getLineCnt lineBoard (x', y') <= getNum lineBoard (x' ,y')

-- ラインを引けるか
allowLines board lineBoard (x, y) top right bottom left =
  allowLine board (x, y) top right bottom left &&
  allowLineTop board lineBoard (x, y) top &&
  allowLineRight board lineBoard (x, y) right &&
  allowLineBottom board lineBoard (x, y) bottom &&
  allowLineLeft board lineBoard (x, y) left

-- ラインを引けるか(this)
allowLine board (x, y) top right bottom left =
  s < 4 && (getNum board (x, y) == s || getNum board (x, y) == 9)
  where s = sum[top, right, bottom, left]

-- ラインを引けるか(top)
allowLineTop board lineBoard (x, y) top = True

-- ラインを引けるか(right)
allowLineRight board lineBoard (x, y) right = True

-- ラインを引けるか(bottom)
allowLineBottom board lineBoard (x, y) bottom = True

-- ラインを引けるか(left)
allowLineLeft board lineBoard (x, y) left = True

-- ラインを引く
putLines board lineBoard (x, y) top right bottom left = lineBoard

-- 解法
--solver :: Board -> LineBoard
solver board = iter board (getInitLineBoard board) (makeIdx board)
  where makeIdx board = [(x, y) | x <- [0..getWidth board-1], y <- [0..getHeight board-1]]
        iter board lineBoard [] = return lineBoard
        iter board lineBoard ((x, y) : idx) = do
          top <- [0, 1]
          right <- [0, 1]
          bottom <- [0, 1]
          left <- [0, 1]
          guard (allowLines board lineBoard (x, y) top right bottom left)
          iter board (putLines board lineBoard (x, y) top right bottom left) idx

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
