{-# LANGUAGE GADTSyntax #-}

module Ex5 where
import Data.Char (isDigit)

digitToInt :: Char -> Int
digitToInt '0' = 0
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9

getNumber :: IO Int
getNumber = getLine >>= (\str ->
             if allDigit str
               then pure (read str :: Int)
               else putStrLn "This input is not a number, please try again!" >> getNumber)
 
allDigit :: String -> Bool
allDigit []     = False
allDigit [c]    = isDigit c
allDigit (c:cs) = isDigit c && allDigit cs
                   
numberToInt :: String -> Int
numberToInt str = foldr (\(c, tens) acc -> (digitToInt c) * tens + acc)
                        0
                        (zip (reverse str) (powerOfTens 0))
 where powerOfTens n = 10^n : powerOfTens (n+1)

---Question 1---
-- str1 "HelloWorld" = "HelloWorld!"
str1 :: String -> String
str1 str = foldr (\ ch acc -> ch : acc) ['!'] str

-- str2 "HelloWorld" = "IfmmpXpsme"
str2 :: String -> String
str2 str = map (\c -> if c`elem` ['a'..'z']
                        then ['a'..'z'] !! ((getIndexS c) +1) 
                        else ['A'..'Z'] !! ((getIndexB c) +1) 
                        ) str
                            where getIndexS c = snd(head(filter (\(ch,index) -> ch == c) smallzip))
                                  getIndexB c = snd(head(filter (\(ch,index) -> ch == c) bigzip)) 
        

-- str3 "HelloWorld" = "HllWrld"
str3 :: String -> String
str3 str = filter (\x -> not(x `elem` ['a','e','i','o','u'])) str

smallzip :: [](Char,Int)
smallzip = zip ['a'..'z'] [0,1..]

bigzip :: [](Char,Int)
bigzip = zip ['A'..'Z'] [0,1..]

---Question 2---
inBounds :: Int -> ((,) Int Int) -> Bool
inBounds x (xMax, xMin) = (x <= xMax) && (x >= xMin)

getinBounds :: Int -> Int -> IO Int
getinBounds x y = putStr ("Please type in a number between " ++ show x ++ " and "++ show y ++ ".\n") 
                >> getNumber >>= (\n -> if inBounds n (x,y)
                                          then pure(n)
                                          else putStr "The number does not meet the given bounds, please try again.\n" >> getinBounds x y) 
    
---Question 4---
guessNumber :: Int -> IO ()
guessNumber num = guessNumber' num 0 


guessNumber' :: Int -> Int -> IO ()
guessNumber' num chance = if chance < 3
                            then putStrLn "Let's try to guess the number!" >> 
                                 getNumber >>= (\guess -> if guess == num
                                                            then  putStr (show num ++ "\n")
                                                            else if guess < num 
                                                                    then putStrLn "Your guess is too small. Try again." >> guessNumber' num (chance+1) 
                                                                    else putStrLn "Your guess is too large. Try again." >> guessNumber' num (chance+1))
                            else putStrLn "NO chances Left. Tuff LUCK."

---guessNumber :: Int -> IO ()
---guessNumber num = putStrLn "Let's try to guess the number!" >> 
---                  getNumber >>= (\guess -> if guess == num
---                                            then  putStr (show num ++ "\n")
---                                            else if guess < num 
---                                                    then putStrLn "Your guess is too small. Try again." >> guessNumber num
---                                                    else putStrLn "Your guess is too large. Try again." >> guessNumber num)

testt :: IO Int
testt = pure (read "0" :: Int)

main :: IO ()
main = do
  let x = read "271" :: Integer
  print x

---Question 7 ---

type Set = Int -> Bool

empty :: Set
empty = \_ -> False

isInSet :: Set
isInSet = \x -> True

isElem :: Int -> Set -> Bool
isElem val isInSet = isInSet val

--- Paper ---
displayMenu :: [String] -> IO ()
displayMenu str= displayMenu' str 1

displayMenu' :: [String] -> Int -> IO ()
displayMenu' [] num = putStrLn ""
displayMenu' (st:sts) num = putStrLn ("("++ show num ++")  " ++ st ) >> displayMenu' sts (num+1)


---Rock Paper Scissor---
data Move = Rock | Paper | Scissors
  deriving Show

getMove :: IO Move
getMove = putStrLn "Rock o, paper [], scissors 8<? ">> getLine >>= 
          (\str -> if str == "o" 
                     then pure Rock
                     else if str == "[]"
                            then pure Paper
                            else if str == "8<"
                                  then pure Scissors
                                  else putStrLn "Invalid move!" >> getMove)

against :: Move -> Move -> Ordering
against Rock Rock = EQ
against Paper Paper = EQ
against Scissors Scissors = EQ
against Rock Paper = GT
against Scissors Rock = GT
against Paper Scissors = GT
against Rock Scissors = LT
against Paper Rock = LT
against Scissors Paper = LT

rndMove :: IO Move
rndMove = pure Rock

play :: IO ()
play = putStrLn "Let's play rock, paper, scissors!" >> getMove >>= 
                                                      (\x -> rndMove >>= 
                                                        (\y -> putStrLn ( show x ++ " VS " ++ show y ) >> if against x y == EQ then putStrLn"Draw!" >> play else if against x y == GT then putStrLn "You Lose!" else putStrLn"You Win!"  
                                                        ))





