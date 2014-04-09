import Data.List
distinct :: [Int] -> [Int] -> [Int]
distinct xs [] = xs	-- foldr1 (+) xs 
distinct xs (y:ys)
		| y `elem` xs 	= distinct xs ys
		| otherwise 	= distinct (xs++[y]) ys


subseq :: [Int] -> [[Int]] -> [[Int]]
subseq [] xs = filter (not . null) xs
subseq ys xs = subseq (init ys) (xs ++ (tails ys))
	
indexes :: [Int] -> [Int] -> (Int, Int)	-- ys är orginallistan, xs skickar vi in
indexes ys xs = (head (elemIndices (head xs) ys), head (elemIndices (last xs) ys))

sumlist :: [Int] -> 	
	
	
-- filter (not . null) (inits ys) 
-- för att summera ihop listor : använd sum eller foldr1.
-- för att get ut de k summorna : använda take.
-- för index: elemIndices, för första: head och för sista: tail.
-- 

-- filter (not . null) (tails [1,2,3,4]) => [[1,2,3,4],[2,3,4],[3,4],[4]]
-- filter (not . null) (tails [1,2,3]) 	 => [[1,2,3],[2,3],[3]]
-- filter (not . null) (tails [1,2]) 	 => [[1,2],[2]]
-- filter (not . null) (tails [1]) 	 	 => [[1]
