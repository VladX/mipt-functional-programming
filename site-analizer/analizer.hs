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

pageRank d url edges edgesTo =
	let pageRank' memorize d url edges edgesTo = case M.lookup url memorize of
		Nothing -> let
			numOfEdges=(fromIntegral (M.size edges))
			degree x=fromIntegral (max 1 (S.size (M.findWithDefault S.empty x edges)))
			(sumPr,m)=S.foldr (
					\x (acc, mem)->let (p,m)=pageRank' (M.insert url 0.0 mem) d x edges edgesTo in
					((p / (degree x)) + acc, m)
				) (1.0/(degree url), memorize) (M.findWithDefault S.empty url edgesTo)
			pr=((1.0-d)/numOfEdges)+d*sumPr
			in (pr, M.insert url pr m)
		Just x -> (x, memorize)
	in fst (pageRank' M.empty d url edges edgesTo)

analize :: String -> IO [(String, Float, Float)]
analize url = do
	let analize' visited answer edges edgesTo url = do
		let (domain, path) = splitUrl url
		putStrLn ("Processing URL: " ++ url)
		links <- getLinks url
		let filteredLinks = filter (\x->domain==(getDomain (fst x))) (map (\(x,y)->(makeAbsolute domain x, y)) links) -- Оставляем только ссылки на одинаковый домен
		let adjustAnswer (l, w) m = M.insertWith (+) (getPath l) w m
		let insertEdge (l, _) m = case M.lookup path m of
			Nothing -> M.insert path (S.singleton (getPath l)) m
			Just x -> M.insert path (S.insert (getPath l) x) m
		let insertEdgeTo (l, _) m = let p=(getPath l) in case M.lookup p m of
			Nothing -> M.insert p (S.singleton path) m
			Just x -> M.insert p (S.insert path x) m
		let traverseLinksGraph m l = let (v, a, e, et) = m in
			if S.member (getPath l) v -- Только ссылки, в которых мы ещё не были
				then return m
				else analize' v a e et l
		foldM traverseLinksGraph (S.insert path visited, foldr adjustAnswer answer filteredLinks, foldr insertEdge edges filteredLinks, foldr insertEdgeTo edgesTo filteredLinks) (map fst filteredLinks)
	(_,ans,edges,edgesTo) <- analize' S.empty M.empty M.empty M.empty url
	h <- openFile "graph.dot" WriteMode
	hPutStrLn h "digraph G {"
	mapM_ (\(x,y)->mapM_ (\y->hPutStrLn h ("\"" ++ x ++ "\" -> \"" ++ y ++ "\";")) (S.toList y)) (M.toList edges)
	hPutStrLn h "}"
	hClose h
	return (DL.sortBy (\(_,a,_) (_,b,_)->b `compare` a) (map (\(x,y)->(x,y,pageRank 0.85 x edges edgesTo)) (M.toList ans)))

main = do
	putStrLn "Page URL:"
	url <- getLine
	links <- analize url
	mapM_ (putStrLn . show) links