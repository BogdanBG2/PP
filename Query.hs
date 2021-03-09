module Query where

import UserInfo
import Rating
import Movie
import Data.List
import Data.List.Split

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = String
type LnSeparator = String

--------------------------------------------------------------------------
-- TODO 1 : READ
--------------------------------------------------------------------------
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table colsep lnsep s = Table
                            (head (map (splitOn colsep) (reverse $ tail $ reverse
                              $ (splitOn lnsep s)))) 
                            (tail (map (splitOn colsep) (reverse $ tail $ reverse
                              $ (splitOn lnsep s))))

user_info = read_table "|" "\n" UserInfo.user_info_str
rating = read_table " " "\n" Rating.rating_str
movie = read_table "|" "\n" Movie.movie_str

--------------------------------------------------------------------------
-- TODO 2 : SHOW / PRINT
--------------------------------------------------------------------------

-- extract matrix elements: line element, line, column
getLineElement :: Int -> [String] -> String
getLineElement _ [] = ""
getLineElement i (x:xs)
    | i == 1 = x
    | otherwise = (getLineElement (i-1) xs)

getMatrixLine :: Int -> [[String]] -> [String]
getMatrixLine _ [] = []
getMatrixLine i (l:ls)
    | i == 1 = l
    | otherwise = (getMatrixLine (i-1) ls)

getMatrixColumn :: Int -> [[String]] -> [String]
getMatrixColumn _ [] = []
getMatrixColumn j (l:ls) = (getLineElement j l):(getMatrixColumn j ls)

{-
   addWalls -> adds '|' between fields
   lineLength -> How long must the lower and upper boundaries be?
   horizLine -> output line separator
   aux1 -> gets the length of the longest string in given column of a given matrix
   aux2 -> returns a list with all maximum lengths of each column
   aux3 -> adds spaces until the length of the result equals the given limit 
   aux4 -> returns the string coresponding to the given table, with added spaces if required
   aux5 -> adds vertical walls on all lines
   aux6 -> adds ceiling, floor and the header-entries seaprator
   aux7 -> output string
-}

getHeader (Table h e) = h
getEntries (Table h e) = e

addWalls :: [String] -> String
addWalls str_list = "|" ++ (foldr(\str acc -> str++"|"++acc) [] str_list) ++ "\n"

lineLength :: Table -> Int
lineLength (Table header entries) = (foldr (+) 0 (map length header)) + (length header) + 1

horizLine :: Table -> String
horizLine t = (replicate (lineLength t) '-') ++ "\n"

aux1 :: Int -> [[String]] -> Int
aux1 i mtrx = head $ reverse $ sort
            $ (map length) $ (getMatrixColumn i mtrx)

aux2 [] = []
aux2 m = [aux1 i m | i <- [1..length(head(m))]]

aux3 _ [] = []
aux3 limit str
        | length(str) < limit = (aux3 (limit-1) str) ++ " "
        | otherwise = str

aux4 (Table header entries) = map (zipWith (aux3) (aux2 (header:entries))) (header:entries)

aux5 t = map (addWalls) (aux4 t)

stringList_to_Table :: [[String]] -> Table
stringList_to_Table (lh:lt) = (Table lh lt)

boundary :: Table -> String
boundary t = horizLine (stringList_to_Table (aux4 t))

aux6 t = (boundary t):(head(aux5 t)):(boundary t):(tail(aux5 t))++(boundary t):[]

aux7 t = foldr (\l acc -> l ++ acc) [] (aux6 t)

instance Show Table where
    show = aux7

--------------------------------------------------------------------------
-- TODO 3 : FILTER CONDITIONS
--------------------------------------------------------------------------

data FilterCondition = Lt Field Integer |
                       Eq Field String |
                       In Field [String] |
                       Not FilterCondition

-----------------------------------------------
-- Select -------------------------------------
-- SelectLimit --------------------------------
-----------------------------------------------
{-
  IMPORTANT NOTE : the indexation will begin at 1
  getColumnIndices -> gets the indices of the given fields
  aux1_2 -> analog
  aux2_2 -> returns the list of entries with the given fields 
  aux3_2 -> transposes the aux2_2 result, so that it can be converted to a table
  transp -> returns the transpose of a given matrix
  rmdups -> removes the duplicated elements of a list
  aux4_2 -> returns a list of the given field from all the entries of a given table
  entries_{CONDITION} -> returns a list of entries that respect the given condition
-}

getColumnIndices :: Table -> [String] -> [Int]
getColumnIndices t new_head = map (\(Just x) -> x+1) (map (`elemIndex` (getHeader t)) new_head)

aux1_2 t new_head = getColumnIndices t new_head
aux2_2 t new_head = zipWith (getMatrixColumn) ak (replicate (length ak) (getEntries t))
      where ak = aux1_2 t new_head
aux3_2 t new_head = transp (aux2_2 t new_head)

transp :: [[a]] -> [[a]]
transp ([]:_) = []
transp m = (map head m) : (transp (map tail m))

-----------------------------------------------------------------------------------------------
-- Filter
-----------------------------------------------------------------------------------------------
getColumnIndex :: Table -> String -> Int
getColumnIndex t field = (\(Just x) -> x+1) $ (elemIndex field (getHeader t))

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups [x] = [x]
rmdups (x:xs) = x:[k | k<-rmdups(xs), k /= x]

aux4_2 :: Table -> String -> [String]
aux4_2 t field = getMatrixColumn i (getEntries t)
               where i = (getColumnIndex t field)

------------------
-- Lt String [String] Table
------------------
entriesLT :: String -> Integer -> Table -> [Entry]
entriesLT field limit t = map ((getEntries t) !!) (sort $ foldr (\xs acc -> xs++acc) []
                        $ map (`elemIndices` list) $ rmdups $ filter (< limit) list)
                        where list = map (read::String->Integer) (aux4_2 t field)

------------------
-- Eq String String Table
------------------
entriesEQ :: String -> String -> Table -> [Entry]
entriesEQ field term t = map ((getEntries t) !!) (sort $ foldr (\xs acc -> xs++acc) []
                       $ map (`elemIndices` list) $ rmdups $ filter (== term) list)
                       where list = aux4_2 t field

------------------
-- In String [String] Table
------------------
entriesIN :: String -> [String] -> Table -> [Entry]
entriesIN field entries t = map ((getEntries t) !!) (sort $ foldr (\xs acc -> xs++acc) []
                          $ map (`elemIndices` list) $ rmdups $ filter (`elem` entries) list)
                          where list = aux4_2 t field

------------------
-- Not (each condition is taken separately)
------------------

entries_NOTLT :: String -> Integer -> Table -> [Entry]
entries_NOTLT field limit t = map ((getEntries t) !!) (sort $ foldr (\xs acc -> xs++acc) []
                            $ map (`elemIndices` list) $ rmdups $ filter (>= limit) list)
                            where list = map (read::String->Integer) (aux4_2 t field)

entries_NOTEQ :: String -> String -> Table -> [Entry]
entries_NOTEQ field term t = map ((getEntries t) !!) (sort $ foldr (\xs acc -> xs++acc) []
                           $ map (`elemIndices` list) $ rmdups $ filter (/= term) list)
                           where list = aux4_2 t field

entries_NOTIN :: String -> [String] -> Table -> [Entry]
entries_NOTIN field entries t = map ((getEntries t) !!) (sort $ foldr (\xs acc -> xs++acc) []
                              $ map (`elemIndices` list) $ rmdups 
                              $ filter (`notElem` entries) list)
                              where list = aux4_2 t field

--------------------------------------------------------------------------
-- TODO 4 : EVAL (QUERY)
--------------------------------------------------------------------------

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom table) = table

eval (Select field_list q) = Table (field_list) 
                          (aux3_2 (eval q) field_list)

eval (SelectLimit field_list limit q) = Table (field_list) 
                                      (take (fromIntegral limit) (aux3_2 (eval q) field_list))

eval (Filter (Lt field limit) q) = Table (getHeader (eval q)) 
                                   (entriesLT field (fromIntegral limit) (eval q))
eval (Filter (Eq field term) q) = Table (getHeader (eval q)) 
                                   (entriesEQ field term (eval q))
eval (Filter (In field entry_list) q) = Table (getHeader (eval q)) 
                                         (entriesIN field entry_list (eval q))

eval (Filter (Not (Lt field limit)) q) = Table (getHeader (eval q))
                                          (entries_NOTLT field (fromIntegral limit) (eval q))
eval (Filter (Not (Eq field term)) q) = Table (getHeader (eval q))
                                         (entries_NOTEQ field term (eval q))
eval (Filter (Not (In field entry_list)) q) = Table (getHeader (eval q)) 
                                               (entries_NOTIN field entry_list (eval q))

eval (q1 :|| q2) = Table (getHeader (eval q1)) 
                   ( rmdups ( (getEntries (eval q1)) ++ ( getEntries (eval q2)) ) )

--------------------------------------------------------------------------
-- TODO 5 : SPECIAL FUNCTIONS
--------------------------------------------------------------------------

{-
  getZoneOf -> gets the zone of a given entry
  aux1_3 -> constructs a table with all the entries from a given zone
-}

getZoneOf s = head (filter (\l -> head l == s) (getEntries user_info)) !! 4
aux1_3 s = eval 
         $ Select ["user_id", "occupation"] 
         $ Filter (Eq "zone" (getZoneOf s)) 
         $ Atom user_info

same_zone :: String -> Query
same_zone s = Atom (Table (getHeader t) (filter (\l -> head l /= s) (getEntries t))) 
            where t = aux1_3 s

male_within_age :: Integer -> Integer -> Query
male_within_age x y = Select ["occupation", "zone"] 
                    $ Filter (Not (In "age" [show x])) 
                    $ Filter (Not (Lt "age" x))
                    $ Filter (Lt "age" y)
                    $ Filter (Eq "sex" "M")
                    $ Atom user_info

-- ALTERNATE SOLUTION (slightly less efficient)
{-
male_within_age x y = Select ["occupation", "zone"]
                    $ Filter (In "age" (map show [(x+1)..(y-1)]))
                    $ Filter (Eq "sex" "M") 
                    $ Atom user_info
-}

mixed :: [String] -> [String] -> Integer -> Query
mixed zone_list occupation_list max_age = Select ["user_id"] 
                                        $ Filter (In "zone" zone_list)
                                        $ Filter (In "occupation" occupation_list)
                                        $ Filter (Lt "age" max_age) $ Atom user_info