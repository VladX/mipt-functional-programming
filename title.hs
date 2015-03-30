import System.IO
import Data.Text
import Data.Char
import Data.List

byWords = (`elem` ['.', ' ', ',', ';', ':', '?', '!', '"', '\r', '\n'])
bySentences = (`elem` ['.'])
splitBy by contents = Data.List.filter (/=empty) (split by (pack contents))
filterNames words x = (isUpper (Data.Text.head x)) && not (elem (Data.Text.toLower x) words)
countConnections x y sentences = Data.List.foldr connected 0 sentences where
	connected e cnt = if (Data.Text.isInfixOf x e) && (Data.Text.isInfixOf y e) then cnt+1 else cnt

main = do
	contents <- readFile "text.txt"
	let words = splitBy byWords contents
	let sentences = splitBy bySentences contents
	let names = nub (Data.List.filter (filterNames words) words)
	let connections = [(n1, n2, conn) | n1 <- names, n2 <- names, n1 /= n2, let conn = countConnections n1 n2 sentences]
	mapM_ (putStrLn . show) (sortBy (\x y->let ((x1,x2,x3),(y1,y2,y3))=(x,y) in y3 `compare` x3) connections)