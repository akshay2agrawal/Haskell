{-# LANGUAGE GADTSyntax #-}

add x = x + x 
addnum :: (Num a) => a -> a
addnum x = x*2

ex1 :: (Num a) => Char -> a
ex1 'a' = 1
ex1 _ = 0

double x = x*2

x' :: (Char,Int)
x' = ('s',2)

tail' :: [a] -> [a]
tail' (_:xs) = xs

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

head' :: (Num a) => [a] -> a  ---ask when to use Num a ,not used for all cases
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:y:z:_) = x+y+z

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    

--- case expressions example (Acts like a switch case!) can be an alternative for pattern matching where there are couple of cases
--below is an example in pattern matching form
--expressions. These two pieces of code do the same thing and are interchangeable:

headd' :: [a] -> a  
headd' [] = error "No head for empty lists!"  
headd' (x:_) = x

-- the above example converted in case expression
--head'' :: [a] -> a  
--head'' xs = case xs of { [] -> (error "No head for empty lists!") }  
--                      (x:_) -> x  
---syntax written below
---case expression of pattern -> result  
---                   pattern -> result  
---                   pattern -> result  
---                   ...

imp :: Bool -> Bool -> Bool
imp True  True  = True
imp True  False = False
imp False True  = True
imp False False = True



 --infixr 1 ==>
 --(==>) :: Bool -> Bool -> Bool
 --b1 ==> b2 = imp b1 b2

 --data xyz where
 --  bac :: xyz   

-- Type function : ask someone and clear the concept

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)
                  where max x y 
                         | x>y = x
                         | otherwise = y 

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x

    ---  ACoord (XYAxis 2 4) (ACoord (XYAxis 2 4) EmptyC) 

data Coordinate' where
  XYAxis :: Integer -> Integer -> Coordinate'
 deriving Show


data CoordMap where
  EmptyC :: CoordMap
  ACoord :: Coordinate' -> CoordMap -> CoordMap
 deriving Show

coordVal1 :: CoordMap
coordVal1 = EmptyC

combineCoordMap :: CoordMap -> CoordMap -> CoordMap
combineCoordMap EmptyC             cm2 = cm2
combineCoordMap (ACoord coord cm1) cm2 = ACoord coord (combineCoordMap cm1 cm2)

--

replaceTokenInRow :: Token -> Row -> Row
replaceTokenInRow Blank _ = 
    error "replaceTokenInRow: Implement me!"

replaceTokenInField :: Token -> Field -> Field
replaceTokenInField = error "replaceTokenInField: Implement me!"


hasToken :: Token -> Field -> Bool
hasToken t EmptyF = False
hasToken t (AField row field) = hasTokenRow t row || hasToken t field
 where
  hasTokenRow t EmptyR = False
  hasTokenRow t (ARow tok row) = eqToken t tok || hasTokenRow t row
                                
eqToken :: Token -> Token -> Bool
eqToken Blank Blank = True
eqToken Block Block = True
eqToken _     _     = False

----Excersice 3----
--Question 1---
data IntList where
      Nil  :: IntList
      Cons :: Integer -> IntList -> IntList

--Quesiton 2--

mapList :: (Integer -> Integer) -> IntList -> IntList
mapList f Nil         = Nil
mapList f Cons x intL = Cons f x (mapList f intL)

filterList :: (Integer -> Bool) -> IntList -> IntList
filterList pred Nil = Nil
filterList pred Cons x intL = if pred x 
                                then Cons x filterList pred intL
                                else filterList pred intL

--Question 3--

incList :: IntList -> IntList
incList iList = mapList func iList
 where func = (+1)

greater5List :: IntList -> IntList
greater5List iList = filterList pred iList
 where pred = (>5)


incList' :: IntList -> IntList
 incList' iList = mapList (\x -> x+1) iList

greater5List' :: IntList -> IntList
greater5List' iList = filterList (>5) iList

--Question 4--
  data Token where
    Blank :: Token
    Block :: Token
   deriving show
                                   
  data Row where
    EmptyR :: Row
    ARow   :: Token -> Row -> Row

  data Field where
    EmptyF :: Field
    AField :: Row -> Field -> Field

mapRow :: (Ex.Token -> Ex.Token) -> Ex.Row -> Ex.Row
mapRow f Ex.EmptyR = Ex.EmptyR
mapRow f (Ex.ARow t rrow) = Ex.Arow (f t) (mapRow f rrow) 
>
mapField :: (Ex.Row -> Ex.Row) -> Ex.Field -> Ex.Field
mapField f Ex.EmptyF = Ex.EmptyF
mapField f (Ex.AField r rf)= Ex.AField (f r) (Ex.mapField f rf)
>
replacet :: 

replaceTokenInRow :: Ex.Token -> Ex.Row -> Ex.Row
replaceTokenInRow t row = mapRow replacet row  
                                    where replacet _ = t

---Remember whenever you are using maps and you write a func_name variable_name(usually a list) ,
---the first element of the list is passed as the input parameter to your function. The function works on that element and if any other variable is to be used in the output or the calculation part you can simply use from the functions input parameters.
--- one does not need to pass it in the function that map contains.
 
replaceTokenInField :: Ex.Token -> Ex.Field -> Ex.Field
replaceTokenInField t field = mapField (\row -> replaceTokenInRow t row ) field

---Question 5---

replaceToken :: Ex.Token -> Field Ex.Token -> Field Ex.Token
replaceToken tok field = map (\row -> map (\t -> tok) row) field


---Question 6---
lookupCoord :: Coord -> Figure token -> Maybe token
lookupCoord cd ts = filter (\(x,_) -> x==c  d) ts
--Or --- lookupCoord cd ((coord,_):fs) = filter (\coord -> coord == cd) fs

---Question 7 ---
blocks :: Coords -> Figure Ex.Token
blocks cds =  map (\cd -> (cd,Ex.Block)) cds


repeatValue :: a -> [] a
repeatValue x = []x ++ repeatValue x

replicateValue :: Int -> a -> [] a
replicateValue n a = if n==0 then [] else a:replicateValue n-1 a
