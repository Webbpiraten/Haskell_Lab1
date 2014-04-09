import Data.List
distinct :: [Int] -> [Int] -> [Int]
distinct xs [] = xs	-- foldr1 (+) xs 
distinct xs (y:ys)
		| y `elem` xs 	= distinct xs ys
		| otherwise 	= distinct (xs++[y]) ys

subseq :: [Int] -> [[Int]] -> [[Int]]
subseq [] xs = filter (not . null) xs
subseq ys xs = subseq (init ys) (xs ++ (tails ys))
	
tripple :: [Int] -> [Int] -> (Int, Int, Int) -- ys är orginallistan, xs ska vi få index o sum
tripple ys xs = (sum xs , head (elemIndices (head xs) ys)+1, head (elemIndices (last xs) ys)+1)

listtrip :: [[Int]] -> [(Int, Int, Int)] -> [Int] -> [(Int, Int, Int)]
listtrip [] xs zs = reverse (sort xs)
listtrip ys xs zs = listtrip (tail ys) (xs ++ [(tripple zs (head ys))]) zs  

kmaxsubunique :: [Int] -> Int -> [(Int, Int, Int)]
kmaxsubunique ys k = take k (listtrip (subseq (distinct [] ys) [[]]) [] (distinct [] ys))
	
-- filter (not . null) (inits ys) 
-- för att summera ihop listor : använd sum eller foldr1.
-- för att get ut de k summorna : använda take.
-- för index: elemIndices, för första: head och för sista: tail.

-- filter (not . null) (tails [1,2,3,4]) => [[1,2,3,4],[2,3,4],[3,4],[4]]
-- filter (not . null) (tails [1,2,3]) 	 => [[1,2,3],[2,3],[3]]
-- filter (not . null) (tails [1,2]) 	 => [[1,2],[2]]
-- filter (not . null) (tails [1]) 	 	 => [[1]
