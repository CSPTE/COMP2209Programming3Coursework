{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),LetExpr(..),CLExpr(..),
                   calcBBInteractions,
                   solveBB,
                   prettyPrint,
                   parseLet,
                   clTransform,
                   innerRedn1,outerRedn1,innerCLRedn1,outerCLRedn1,compareInnerOuter
                   )
where


-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

import Data.List (group, sort, (\\))
import Data.Char (isDigit, digitToInt)

--instance NFData CLExpr
--instance NFData LetExpr
--instance NFData LamExpr
--instance NFData Marking
--instance NFData Side


-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ]
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)

data Marking2 = Absorbtion | Deflection | DoubleDeflection | NothingHappens deriving (Show,Eq)


calcBBInteractions :: Int -> [(Int, Int)] -> [((Side, Int), Marking)]
calcBBInteractions n atoms = [((side,x), checkEdgeReflection (side,x) n (mathsAtomCoordinates atoms) (get2DCoordinate n side x)) | side <-[North,East,South,West], x <- [1..n]] where
    mathsAtomCoordinates atoms = [(fst y, -snd y) | y <- atoms]
    get2DCoordinate n side x  | side == North = (South, (x,-1))
                              | side == South = (North, (x, -n))
                              | side == West = (East, (1,-x))
                              | side == East = (West, (n, -x))

checkEdgeReflection :: (Side, Int) -> Int -> [(Int, Int)] -> (Side, (Int, Int)) -> Marking
checkEdgeReflection start n atoms (side, (x,y)) | not (null [(x1,y1) | (x1,y1) <- getAllPossibleAbsorbPositions n (getAtomsOnTheEdgeOfGrid n side atoms), (x,y) == (x1,y1)]) = Reflect
                                                | otherwise = rayPath start n atoms (side, (x,y))

getAtomsOnTheEdgeOfGrid :: (Eq a, Num a) => a -> Side -> [(a, a)] -> [(a, a)]
getAtomsOnTheEdgeOfGrid n side (a:atoms) | null atoms && side == North && (snd a == -n) = [a]
                                         | null atoms && side == South && (snd a == -1) = [a]
                                         | null atoms && side == East && (fst a == 1) = [a]
                                         | null atoms && side == West && (fst a == n) = [a]
                                         | null atoms = []
                                         | side == North && (snd a == -n) = a : getAtomsOnTheEdgeOfGrid n side atoms
                                         | side == South && (snd a == -1) = a : getAtomsOnTheEdgeOfGrid n side atoms
                                         | side == East && (fst a == 1) = a : getAtomsOnTheEdgeOfGrid n side atoms
                                         | side == West && (fst a == n) = a : getAtomsOnTheEdgeOfGrid n side atoms
                                         | otherwise = getAtomsOnTheEdgeOfGrid n side atoms

getAllPossibleAbsorbPositions :: (Ord b, Num b) => b -> [(b, b)] -> [(b, b)]
getAllPossibleAbsorbPositions _ [] = []
getAllPossibleAbsorbPositions n atoms = removeImpossibleDirections n (getAbsorbPositions atoms) where
    removeImpossibleDirections n absorb = [(x,y) | (x,y) <- absorb, x > 0 && y < 0 && x <= n && y >= -n]

getAbsorbPositions :: (Num a1, Num a2) => [(a1, a2)] -> [(a1, a2)]
getAbsorbPositions (a:atoms) | null atoms = calculateAbsorbPositions a
                             | otherwise = calculateAbsorbPositions a ++ getAbsorbPositions atoms where
                                 calculateAbsorbPositions (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

rayPath :: (Side, Int) -> Int -> [(Int, Int)] -> (Side, (Int, Int)) -> Marking
rayPath start n atoms (side, (x,y)) | (x < 1 || y > -1 || x > n || y < -n) && (start == (side, exitPosition n side (moveOne side (x,y)))) = Reflect
                                    | (x < 1 || y > -1 || x > n || y < -n) = Path (side, exitPosition n side (moveOne side (x,y)))
                                    --Absorbtions
                                    | atomInteractions n atoms (x,y) == Absorbtion = Absorb
                                    --Double Deflections
                                    | side == North && atomInteractions n atoms (x,y) == DoubleDeflection = rayPath start n atoms (South, moveOne South (x,y))
                                    | side == South && atomInteractions n atoms (x,y) == DoubleDeflection = rayPath start n atoms (North, moveOne North (x,y))
                                    | side == East && atomInteractions n atoms (x,y) == DoubleDeflection = rayPath start n atoms (West, moveOne West (x,y))
                                    | side == West && atomInteractions n atoms (x,y) == DoubleDeflection = rayPath start n atoms (East, moveOne East (x,y))
                                    --Single Deflections
                                    | (side == North || side == South) && atomInteractions n atoms (x,y) == Deflection && atomInteractions n atoms (x - 1, y) /= Absorbtion = rayPath start n atoms (West, moveOne West (x,y))
                                    | (side == North || side == South) && atomInteractions n atoms (x,y) == Deflection = rayPath start n atoms (East, moveOne East (x,y))
                                    | (side == West || side == East) && atomInteractions n atoms (x,y) == Deflection && atomInteractions n atoms (x, y - 1) /= Absorbtion = rayPath start n atoms (South, moveOne South (x,y))
                                    | (side == West || side == East) && atomInteractions n atoms (x,y) == Deflection = rayPath start n atoms (North, moveOne North (x,y))
                                    | otherwise = rayPath start n atoms (side, moveOne side (x,y)) where
                                        exitPosition n side (x,y) | y < 0 = exitPosition n side (x,-y)
                                                                  | side == North || side == South = x
                                                                  | otherwise = y
                                        moveOne side (x,y) | side == North = (x,y+1)
                                                           | side == South = (x, y-1)
                                                           | side == West = (x-1,y)
                                                           | side == East = (x+1,y)

--calculate the interaction of ray with atom
atomInteractions :: (Ord b, Num b) => b -> [(b, b)] -> (b, b) -> Marking2
atomInteractions n atoms (x,y) | not (null [(x1,y1) | (x1,y1) <- getAllPossibleAbsorbPositions n atoms, (x,y) == (x1,y1)]) = Absorbtion
                               | not (null [(x1,y1) | (x1,y1) <- getAllPossibleDoubleDeflectionPositions n atoms, (x,y) == (x1,y1)]) = DoubleDeflection
                               | not (null [(x1,y1) | (x1,y1) <- getAllPossibleSingleDeflectionPositions n atoms, (x,y) == (x1,y1)]) = Deflection
                               | otherwise = NothingHappens
            where getAllPossibleSingleDeflectionPositions n atoms = removeDuplicates (head (([getCornersInTheGrid n atoms] \\ [getAllPossibleAbsorbPositions n atoms]) \\ [getAllPossibleDoubleDeflectionPositions n atoms]))
                  getAllPossibleDoubleDeflectionPositions _ [] = []
                  getAllPossibleDoubleDeflectionPositions n atoms = removeDuplicates [(x,y) | (x,y) <- mergeLists (getDoubleDeflectionCorners atoms), x > 0 && y < 0 && x <= n && y >= -n]

getCornersInTheGrid n atoms = [(x1,y1) | (x1,y1) <- mergeLists ([[(x+1,y+1), (x+1,y-1), (x-1, y-1), (x-1,y+1), (x,y+1), (x,y-1),(x+1,y),(x-1,y)] | (x,y) <- atoms]), x1 > 0 && y1 < 0 && x1 <= n && y1 >= -n]

getDoubleDeflectionCorners atoms = [getSharedCorners (x1,y1) (x2,y2) | (x1,y1) <- atoms, (x2,y2) <- tail atoms, (x1,y1) /= (x2,y2)] where
    getSharedCorners (x1,y1) (x2,y2) = [(a,b) | (a,b) <- getAtomCorners (x1,y1), (a1,b1) <- getAtomCorners (x2,y2), (a,b) == (a1,b1)]
    getAtomCorners (x,y) = [(x+1,y+1), (x+1,y-1), (x-1,y+1), (x-1,y-1)]

mergeLists :: [[a]] -> [a]
mergeLists [] = []
mergeLists (x:xs) | null xs = x
                  | otherwise = x ++ mergeLists xs

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort





-- Challenge 2
-- Solve a Black Box

solveBB :: Int -> Interactions -> Atoms
solveBB _ _ = []





-- Challenge 3
-- Pretty Printing Lambda with Scott Numerals

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int
                deriving (Eq, Show, Read)

data Precision = Free | Fun | Arg | Block
             deriving (Eq, Ord)

prettyPrint :: LamExpr -> String
prettyPrint xs = scottEncoding (printLambda Free xs)

printLambda :: Precision -> LamExpr -> String
printLambda _ (LamVar i) = "x" ++ show i
printLambda p (LamAbs i b) = parentesesDecider (p > Free && p /= Block) ("\\x" ++ show i ++ " -> " ++ printLambda Free b)
printLambda p (LamApp f x) = parentesesDecider (p > Fun) (printLambda Fun f ++ " " ++ printLambda arg x) where
    arg = if p > Free then Arg else Block

parentesesDecider :: Bool -> String -> String
parentesesDecider True v = "(" ++ v ++ ")"
parentesesDecider False v = v

scottEncoding :: [Char] -> [Char]
scottEncoding v = putSpaceBetweenValues (makeHigherNumbers (length mkZeros) mkZeros) where
    mkZeros = makeAllZeros (length (removeParantheses (splitOn ' ' v))) (removeParantheses (splitOn ' ' v))
    putSpaceBetweenValues (x:xs) | length xs == 0 = x
                                 | otherwise = x ++ " " ++ putSpaceBetweenValues xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a [] = []
splitOn a v =  x : splitOn a (drop 1 y) where
    (x,y) = span (/= a) v

removeParantheses :: [[Char]] -> [[Char]]
removeParantheses (x:xs) | (length xs == 0) && (head x == '(') = "(" : [tail x]
                         | (length xs == 0) && (x !! (length x - 1) == ')') = deleteNth (length x - 1) x : [")"]
                         | (length xs == 0) = [x]
                         | head x == '(' = "(" : tail x : removeParantheses xs
                         | x !! (length x - 1) == ')' = deleteLast x : ")" : removeParantheses xs
                         | otherwise = x : removeParantheses xs where
                             deleteLast (x:xs) | length xs == 0 = []
                                               | otherwise = x : deleteLast xs

deleteNth n (x:xs) | n-1 == 0 = xs
                 | otherwise = x : deleteNth (n-1) xs

makeAllZeros :: Int -> [String] -> [[Char]]
makeAllZeros n xs | (n - 5 < 0) = xs
                  | (n - 5 >= 0) && (not (checkForSlashes (xs !! (n-1)))) && (xs !! (n-2) == "->") && (checkForSlashes (xs !! (n-3))) && (xs !! (n-4) == "->") && (checkForSlashes (xs !! (n-5))) = makeAllZeros (length (changeToZero n xs)) (changeToZero n xs)
                  | otherwise = makeAllZeros (n-1) xs where
                      checkForSlashes xs = not (null [x | x <- xs, x == '\\'])
                      changeToZero n xs = take (n - 5) xs ++ ["0"] ++ drop 5 (drop (n - 5) xs)

makeHigherNumbers :: Int -> [String] -> [[Char]]
makeHigherNumbers n xs | (length xs < 2) || n < 2 = xs
                       | (findNumbers (xs !! (n-1))) && (findNumbers (xs !! (n-2))) = makeHigherNumbers (length makeTheNumbers) makeTheNumbers
                       | otherwise = makeHigherNumbers (n-1) xs where
                           findNumbers (x:xs) | null xs = isDigit x
                                              | isDigit x = findNumbers xs
                                              | otherwise = False
                           makeTheNumbers = placeaAtNthSpot (n-1) placeHigherNum delete02
                           delete02 = deleteNth (n-1) delete01
                           delete01 = deleteNth (n-1) xs
                           placeHigherNum = show (read (xs !! (n-1)) + 1)
                           placeaAtNthSpot n x [] = [x]
                           placeaAtNthSpot 0 x ys = x : ys
                           placeaAtNthSpot n x (y:ys) | n > length ys  = y : ys ++ [x]
                                                      | n-1 == 0 = x : y : ys
                                                      | otherwise = y : placeaAtNthSpot (n-1) x ys





-- Challenge 4 
-- Parsing Let Expressions

data LetExpr =  LetApp LetExpr LetExpr | LetDef [([Int], LetExpr)] LetExpr |
                LetFun Int | LetVar Int | LetNum Int
                deriving (Show, Eq)

parseLet :: String -> Maybe LetExpr
parseLet _  = Just (LetNum 0)





-- Challenge 5
-- Encode lambda terms as combinators 

data CLExpr = S | K  | I | CLVar Int | CLApp CLExpr CLExpr
              deriving (Show,Read,Eq)

clTransform :: LamExpr -> CLExpr
clTransform _ =  CLVar 0





-- Challenge 6
-- Compare Innermost and Outermost Reduction for Lambda and Combinators 

outerRedn1 :: LamExpr -> Maybe LamExpr
outerRedn1 _ = Just (LamVar 0)

innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 _ = Just (LamVar 0)

outerCLRedn1 :: CLExpr -> Maybe CLExpr
outerCLRedn1 _ = Just (CLVar 0)

innerCLRedn1 :: CLExpr -> Maybe CLExpr
innerCLRedn1 _ = Just (CLVar 0)

compareInnerOuter :: LamExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Just 0, Just 0, Just 0, Just 0)




