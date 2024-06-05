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
putLine board (-1, _) = board
putLine board (_, -1) = board
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

-- 引いたラインが結合したかをチェックする
checkLineLinked :: LineBoard -> Position -> Bool
checkLineLinked board (x, y) = (sum $ map f makeIdx) == 2
  where makeIdx = 
          if even y then [(x'',y'')|(x'',y'')<-[(1,-1), (2,0), (1, 1), (-1,1), (-2, 0), (-1,-1)],x+x''>=0,x+x''<getWidth board,y+y''>=0,y+y''<getHeight board]
          else [(x'',y'')|(x'',y'')<-[(0,-2), (1,-1), (1,1), (0,2), (-1,1), (-1,-1)],x+x''>=0,x+x''<getWidth board,y+y''>=0,y+y''<getHeight board]
        f (x', y') = getNum board ((x + x'), (y + y'))

-- 解法
--solver :: Board -> LineBoard
-- 1.初期ラインボードを作成する
-- 2.最初の数字の場所を取得する
-- 3.仮のラインを引く
--    →引けるラインがなければ失敗なので前のラインに戻る
-- 4.整合性チェック
--    →Falseならば3へ戻る
-- 5.結合チェック
--    →Trueならば6を実行
-- 6.全数字に対して線が引かれているかチェック
--    →Trueならば回答する
--    →Falseならば次のラインを引く(3)
solver board = solve1 (getInitLineBoard board)
  where solve1 lineBoard = solve2 lineBoard (getFirstNumPosition lineBoard)
        solve2 lineBoard (x, y) = solve3 lineBoard (x, y) makeIdx
        solve3 lineBoard (x, y) ((x', y'):idx) = do
          print "lineBoard:"
          print lineBoard
          let targetX = x + x'
              targetY = y + y'
              tmpLineBoard = putLine lineBoard (targetX, targetY)
              makeTmpIdx = 
                if even targetY then [(1,-1), (2,0), (1, 1), (-1,1), (-2, 0), (-1,-1)]
                else [(0,-2), (1,-1), (1,1), (0,2), (-1,1), (-1,-1)]
          print "targetX:"
          print targetX
          print "targetY:"
          print targetY
          print "tmpLineBoard:"
          print tmpLineBoard
          guard (targetX >= 0)
          print "targetX >= 0"
          guard (targetY >= 0)
          print "targetY >= 0"
          guard (targetX < getWidth lineBoard)
          print "targetX < getWidth lineBoard"
          guard (targetY < getWidth lineBoard)
          print "targetY < getWidth lineBoard"
          guard (lineBoard !! targetY !! targetX == 0)
          print "lineBoard !! targetY !! targetX == 0"
          guard (checkLineIntegrity tmpLineBoard (targetX, targetY))
          print "checkLineIntegrity tmpLineBoard (targetX, targetY)"
          guard (not $ checkLineLinked tmpLineBoard (targetX, targetY))
          print "not $ checkLineLinked tmpLineBoard (targetX, targetY)"
          if checkAllNumSatisfied board tmpLineBoard then return tmpLineBoard
          else solve3 tmpLineBoard (targetX, targetY) makeTmpIdx
        makeIdx = [(0,-1), (1,0), (0,1), (-1,0)]

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
