module Exam where

-- Task two

type Vertex = Char
type Path = [Vertex]
type Graph = Dictionary Vertex [Vertex]
type Dictionary k v = [(k, v)]

keysOf :: Dictionary k v -> [k]
keysOf = map fst
valuesOf = map snd

search :: Eq k => k -> Dictionary k v -> v
search key ((k, v) : pairs)
 | key == k = v
 | otherwise = search key pairs

verticesOf :: Graph -> [Vertex]
verticesOf = keysOf

isVertexOf :: Vertex -> Graph -> Bool
isVertexOf v g = v `elem` (verticesOf g)

childrenOf :: Vertex -> Graph -> [Vertex]
childrenOf = search

hasEdge :: Vertex -> Vertex -> Graph -> Bool
hasEdge u v g = v `elem` (childrenOf u g)

findWord :: String -> Graph -> Bool
findWord [] _ = True
findWord word g = all (`isVertexOf` g) word
                  && verticesFormAPath
 where verticesFormAPath = all isEdge pairs
       pairs = zip word (tail word)
       isEdge (u, v) = hasEdge u v g

quickSort :: Ord item => [item] -> [item]
quickSort [] = []
quickSort (pivot : items) = sortedSmaller ++ (pivot : sortedLarger)
 where sortedSmaller = quickSort $ filter (<= pivot) items
       sortedLarger = quickSort $ filter (> pivot) items

{-
allWords :: Vertex -> Graph -> [Path]
allWords source g = bfs [[source]]
 where bfs paths = bfs extend paths
       extend paths = concat $ map extendPath paths
       extendPath p = map (: p) (childrenOf (head p) g) 
-}
-- Task three

movies :: [Movie]
movies = [(Movie "Batman" 7.5 126), (Movie "Manhattan" 8.0 96),
          (Movie "Alien" 8.5 116), (Movie "Amadeus" 8.3 160)]

data Movie = Movie { titleOf :: String,
                     ratingOf :: Double,
		     durationOf :: Int } deriving (Show)

instance Eq Movie where
 lhs == rhs = (ratingOf lhs) == (ratingOf rhs)

instance Ord Movie where
 compare lhs rhs = compare (ratingOf lhs) (ratingOf rhs)

maxRatedMovie :: [Movie] -> Maybe Movie
maxRatedMovie = maxInList 

extractTitle :: Maybe Movie -> String
extractTitle Nothing = ""
extractTitle (Just m) = titleOf m

bestMovie :: [Movie] -> Int -> String
bestMovie movies maxDuration = extractTitle $ maxRatedMovie filteredMovies
 where filteredMovies = [m | m <- movies, durationOf m <= maxDuration]

subsetsOf :: [item] -> [[item]]
subsetsOf [] = [[]]
subsetsOf (h : t) =
 [singleton ++ subset | subset <- subsetsOf t, singleton <- [[], [h]]]

averageRatingOf :: [Movie] -> Double
averageRatingOf movies = (sum ratings) / (fromIntegral count)
 where ratings = map ratingOf movies
       count = length movies

summedDurationOf :: [Movie] -> Int
summedDurationOf = sum . (map durationOf)

data Pair = Pair [Movie] Int

instance Eq Pair where
 (Pair _ lhs) == (Pair _ rhs) = lhs == rhs

instance Ord Pair where
 compare (Pair _ lhs) (Pair _ rhs) = compare lhs rhs

bestPlaylist :: [Movie] -> Int -> Double -> [Movie]
bestPlaylist movies maxDuration minRating = extractMovies $ maxDurationPair filteredSubsets
 where filteredSubsets =
        [Pair s (summedDurationOf s) | s <- subsetsOf movies, (averageRatingOf s) >= minRating, (summedDurationOf s) <= maxDuration]

extractMovies :: Maybe Pair -> [Movie]
extractMovies Nothing = []
extractMovies (Just (Pair m _)) = m

maxDurationPair :: [Pair] -> Maybe Pair
maxDurationPair = maxInList

maxInList :: Ord item => [item] -> Maybe item
maxInList [] = Nothing
maxInList [item] = Just item
maxInList (item : items) = max (Just item) (maxInList items)
