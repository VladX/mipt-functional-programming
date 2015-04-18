import System.IO
import qualified Control.Parallel as P
import qualified Data.Text as T
import qualified Data.Map as M

data Trie = Trie { wins :: Int, total :: Int, move :: String, child :: M.Map String (Trie) } deriving (Show)

emptyTrie = Trie { wins = 0, total = 0, move = "", child = M.empty }

evenElems [] _ = []
evenElems (x:xs) n = let t = evenElems xs (n+1) in if even n then x:t else t

wWinner winner = if winner=="1-0" then 1 else 0
bWinner winner = if winner=="0-1" then 1 else 0

maxBy1 a b = if (fst a)>(fst b) then a else b

insert win [] t = t
insert win (k:ks) t = let
	ts = child t
	childNode = emptyTrie { move = k }
	newChildren = M.insert k childNode ts
	newWins = (wins t) + win
	newTotal = (total t) + 1
    in case M.lookup k ts of
    	Nothing -> t { wins = newWins, total = newTotal, child = M.insert k (insert win ks childNode) newChildren }
    	Just t' -> t { wins = newWins, total = newTotal, child = M.insert k (insert win ks t') ts }

longestDebut moves t = if (total t) < 2
	then (0, moves)
	else M.foldr (maxBy1.(\x->((fst x)+1,snd x)).(longestDebut ((move t):moves))) (0,moves) (child t)

bestDebut moves t = if (total t) < 2
	then (0.0, moves)
	else maxBy1 ((fromIntegral (wins t))/(fromIntegral (total t)), (move t):moves) (M.foldr (maxBy1.(bestDebut ((move t):moves))) (0.0, moves) (child t))

worstMove moves p t = if (total t) == 0 then (0.0, moves) else let
		cp = (fromIntegral (wins t))/(fromIntegral (total t))
		deltaProb = abs (cp-p)
		in maxBy1 (deltaProb, (move t):moves) (M.foldr (maxBy1.(worstMove ((move t):moves) cp)) (0.0, moves) (child t))

buildTrie lines =
	let buildTrie' whiteTrie blackTrie lines = case lines of
		[] -> (whiteTrie, blackTrie)
		lines -> let (winner, moves) = (lines!!0, words (lines!!1))
			in buildTrie' (insert (wWinner winner) (evenElems moves 0) whiteTrie) (insert (bWinner winner) (evenElems moves 1) blackTrie) (drop 2 lines)
	in buildTrie' emptyTrie emptyTrie lines

prettyPrint w b = do
	let print x = let (val, moves)=(fst x, tail (reverse (snd x))) in putStrLn ((show val) ++ "; Ходы: " ++ (show moves))
	print w
	print b

main = do
	contents <- readFile "moves.txt"
	let lines = map T.unpack (T.splitOn (T.pack "\n") (T.pack contents))
	let (white, black) = buildTrie lines
	putStrLn "Самый длинный дебют (белые/чёрные):"
	let (w,b)=P.par x (P.pseq y (x,y)) where
		x=longestDebut [] white
		y=longestDebut [] black
	prettyPrint w b
	putStrLn "---\nДебюты, имееющие наибольшую вероятность выигрыша (белые/чёрные):"
	let (w,b)=P.par x (P.pseq y (x,y)) where
		x=bestDebut [] white
		y=bestDebut [] black
	prettyPrint w b
	putStrLn "---\nСамый плохой ход (белые/чёрные):"
	let (w,b)=P.par x (P.pseq y (x,y)) where
		x=worstMove [] 0.5 white
		y=worstMove [] 0.5 black
	prettyPrint w b