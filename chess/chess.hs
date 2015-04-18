import System.IO
import qualified Data.Text as T
import qualified Data.Map as M

data Trie = Trie { wins :: Int, total :: Int, child :: M.Map String (Trie) } deriving (Show)

emptyTrie = Trie { wins = 0, total = 0, child = M.empty }

evenElems [] _ = []
evenElems (x:xs) n = let t = evenElems xs (n+1) in if even n then x:t else t

wWinner winner = if winner=="1-0" then 1 else 0
bWinner winner = if winner=="0-1" then 1 else 0

insert win [] t = t
insert win (k:ks) t = let
	ts = child t
	childNode = emptyTrie
	newChildren = M.insert k childNode ts
	newWins = (wins t) + win
	newTotal = (total t) + 1
    in case M.lookup k ts of
    	Nothing -> t { wins = newWins, total = newTotal, child = M.insert k (insert win ks childNode) newChildren }
    	Just t' -> t { wins = newWins, total = newTotal, child = M.insert k (insert win ks t') ts }

longestDebut t = if (total t) < 2 then 0 else M.foldr (max.(+1).longestDebut) 0 (child t)

bestDebut t = if (total t) < 2 then 0.0 else max ((fromIntegral (wins t))/(fromIntegral (total t))) (M.foldr (max.bestDebut) 0.0 (child t))

buildTrie lines =
	let buildTrie' whiteTrie blackTrie lines = case lines of
		[] -> (whiteTrie, blackTrie)
		lines -> let (winner, moves) = (lines!!0, words (lines!!1))
			in buildTrie' (insert (wWinner winner) (evenElems moves 0) whiteTrie) (insert (bWinner winner) (evenElems moves 1) blackTrie) (drop 2 lines)
	in buildTrie' emptyTrie emptyTrie lines

main = do
	contents <- readFile "moves.txt"
	let lines = map T.unpack (T.splitOn (T.pack "\n") (T.pack contents))
	let (white, black) = buildTrie lines
	putStrLn "Самый длинный дебют (белые/чёрные):"
	putStrLn (show (longestDebut white))
	putStrLn (show (longestDebut black))
	putStrLn "Дебюты, имееющие наибольшую вероятность выигрыша (белые/чёрные):"
	putStrLn (show (bestDebut white))
	putStrLn (show (bestDebut black))