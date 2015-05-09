{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Monad
import Network.HTTP.Conduit
import Network (withSocketsDo)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument, element, attribute, parent, hasAttribute, ($//), (>=>), (&|))
import qualified Data.Text as T
import qualified Text.Regex as R
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.List as DL

cursorFor :: String -> IO Cursor
cursorFor u = do
	page <- withSocketsDo $ simpleHttp u
	return $ fromDocument $ parseLBS page

splitUrl :: String -> (String, String)
splitUrl url = case (R.matchRegex (R.mkRegex "^(http://|https://|//)(www\\.)?([-_a-zA-Z0-9\\.]+)/?([^#]*).*$") url) of
	Nothing -> ("", url)
	Just x -> (x!!2, "/" ++ (x!!3))

getDomain = fst . splitUrl
getPath = snd . splitUrl

getDepth n = case parent n of
	[] -> 1
	(x:xs) -> 1+(getDepth x)

getLinks :: String -> IO [(String, Float)]
getLinks url = do
	cursor <- cursorFor url
	return $ cursor $// element "a" >=> hasAttribute "href" &| (\l->(T.unpack (T.concat (attribute "href" l)), 1.0/(getDepth l)))

makeAbsolute domain link = if (take 2 link)=="//"
	then ("http:" ++ link)
	else if (take 1 link)=="/"
		then ("http://" ++ domain ++ link)
		else link

analize :: String -> IO [(String, Float)]
analize url = do
	let analize' visited answer edges url = do
		let (domain, path) = splitUrl url
		putStrLn ("Processing URL: " ++ url)
		links <- getLinks url
		let filteredLinks = filter (\x->domain==(getDomain (fst x))) (map (\(x,y)->(makeAbsolute domain x, y)) links) -- Оставляем только ссылки на одинаковый домен
		let adjustAnswer (l, w) m = M.insertWith (+) (getPath l) w m
		let insertEdge (l, _) m = M.insert path (getPath l) m
		let traverseLinksGraph m l = let (v, a, e) = m in
			if S.member (getPath l) v -- Только ссылки, в которых мы ещё не были
				then return m
				else analize' v a e l
		foldM traverseLinksGraph (S.insert path visited, foldr adjustAnswer answer filteredLinks, foldr insertEdge edges filteredLinks) (map fst filteredLinks)
	(_,ans,edges) <- analize' S.empty M.empty M.empty url
	h <- openFile "graph.dot" WriteMode
	hPutStrLn h "digraph G {"
	mapM_ (\(x,y)->hPutStrLn h ("\"" ++ x ++ "\" -> \"" ++ y ++ "\";")) (M.toList edges)
	hPutStrLn h "}"
	hClose h
	return (DL.sortBy (\(_,a) (_,b)->b `compare` a) (M.toList ans))

main = do
	putStrLn "Page URL:"
	url <- getLine
	links <- analize url
	mapM_ (putStrLn . show) links