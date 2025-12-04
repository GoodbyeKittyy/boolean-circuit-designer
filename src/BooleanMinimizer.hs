-- Boolean Logic Circuit Minimizer using Quine-McCluskey Algorithm
-- Haskell Implementation for Expression Simplification and Optimization

module BooleanMinimizer where

import Data.List (sort, nub, subsequences, (\\), intercalate)
import Data.Bits (xor, (.&.), (.|.), complement, shiftR)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Data types for boolean expressions
data BoolExpr = Var Char
              | Not BoolExpr
              | And BoolExpr BoolExpr
              | Or BoolExpr BoolExpr
              | Xor BoolExpr BoolExpr
              deriving (Eq, Show)

-- Implicant representation: (binary value, mask, variables covered)
data Implicant = Implicant {
    value :: Int,
    mask :: Int,
    minterms :: [Int]
} deriving (Eq, Show)

-- Truth table entry
data TruthEntry = TruthEntry {
    inputs :: Map.Map Char Bool,
    output :: Bool
} deriving (Show)

-- Evaluate boolean expression
eval :: BoolExpr -> Map.Map Char Bool -> Bool
eval (Var c) env = Map.findWithDefault False c env
eval (Not e) env = not (eval e env)
eval (And e1 e2) env = eval e1 env && eval e2 env
eval (Or e1 e2) env = eval e1 env || eval e2 env
eval (Xor e1 e2) env = eval e1 env /= eval e2 env

-- Generate all variable assignments
allAssignments :: [Char] -> [Map.Map Char Bool]
allAssignments [] = [Map.empty]
allAssignments (v:vs) = do
    rest <- allAssignments vs
    [Map.insert v False rest, Map.insert v True rest]

-- Generate truth table
generateTruthTable :: BoolExpr -> [Char] -> [TruthEntry]
generateTruthTable expr vars = 
    let assignments = allAssignments (sort vars)
    in map (\env -> TruthEntry env (eval expr env)) assignments

-- Extract minterms from truth table
extractMinterms :: [TruthEntry] -> [Char] -> [Int]
extractMinterms entries vars = 
    let indexed = zip [0..] entries
        trueEntries = filter (\(_, e) -> output e) indexed
    in map fst trueEntries

-- Count number of 1s in binary representation
countOnes :: Int -> Int
countOnes n = length $ filter (== '1') (toBinary n)

-- Convert integer to binary string
toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = reverse $ toBin n
    where
        toBin 0 = ""
        toBin x = (if x .&. 1 == 1 then '1' else '0') : toBin (shiftR x 1)

-- Pad binary string to given length
padBinary :: Int -> String -> String
padBinary len s = replicate (len - length s) '0' ++ s

-- Check if two implicants differ by one bit
differByOne :: Int -> Int -> Int -> Maybe Implicant
differByOne val1 val2 mask1 = 
    let diff = xor val1 val2
        masked = diff .&. mask1
    in if countOnes masked == 1
       then Just $ Implicant (val1 .&. complement diff) (mask1 .&. complement diff) []
       else Nothing

-- Combine implicants
combineImplicants :: [Implicant] -> [Implicant]
combineImplicants implicants = 
    let grouped = groupByOnes implicants
        combined = concatMap combinePairs (pairs implicants)
    in nub combined
    where
        pairs xs = [(x, y) | x <- xs, y <- xs, x /= y]
        combinePairs (i1, i2) = 
            case differByOne (value i1) (value i2) (mask i1) of
                Just imp -> [imp { minterms = nub (minterms i1 ++ minterms i2) }]
                Nothing -> []

-- Group implicants by number of ones
groupByOnes :: [Implicant] -> [[Implicant]]
groupByOnes implicants = 
    let maxOnes = maximum $ map (countOnes . value) implicants
        groups = [filter (\i -> countOnes (value i) == n) implicants | n <- [0..maxOnes]]
    in filter (not . null) groups

-- Quine-McCluskey algorithm
quineMcCluskey :: [Int] -> Int -> [Implicant]
quineMcCluskey minterms numVars = 
    let initial = [Implicant m (2^numVars - 1) [m] | m <- minterms]
        result = iterate combineImplicants initial
        primeImplicants = head $ dropWhile (\x -> combineImplicants x /= x) result
    in primeImplicants

-- Find essential prime implicants
findEssentialImplicants :: [Implicant] -> [Int] -> [Implicant]
findEssentialImplicants implicants minterms = 
    let coverage = [(m, filter (covers m) implicants) | m <- minterms]
        essential = [head imps | (_, imps) <- coverage, length imps == 1]
    in nub essential
    where
        covers minterm imp = minterm `elem` minterms imp

-- Convert implicant to term string
implicantToTerm :: Implicant -> [Char] -> String
implicantToTerm (Implicant val msk _) vars = 
    let numVars = length vars
        bits = [if testBit msk i then Just (testBit val i) else Nothing 
                | i <- reverse [0..numVars-1]]
        terms = [case (v, bit) of
                    (_, Nothing) -> ""
                    (var, Just True) -> [var]
                    (var, Just False) -> var : "'"
                 | (var, bit) <- zip vars bits, bit /= Nothing]
    in intercalate "" (filter (not . null) terms)
    where
        testBit n i = (n .&. (1 `shiftR` i)) /= 0

-- Minimize boolean expression
minimize :: BoolExpr -> [Char] -> String
minimize expr vars = 
    let truthTable = generateTruthTable expr vars
        minterms = extractMinterms truthTable vars
        numVars = length vars
    in if null minterms
       then "0"
       else if length minterms == 2^numVars
            then "1"
            else let primes = quineMcCluskey minterms numVars
                     essential = findEssentialImplicants primes minterms
                     terms = map (\i -> implicantToTerm i vars) essential
                 in intercalate " + " (filter (not . null) terms)

-- Circuit complexity metrics
data ComplexityMetrics = ComplexityMetrics {
    numGates :: Int,
    numLiterals :: Int,
    depth :: Int,
    fanout :: Map.Map Char Int
} deriving (Show)

-- Calculate circuit complexity
calculateComplexity :: BoolExpr -> ComplexityMetrics
calculateComplexity expr = 
    ComplexityMetrics {
        numGates = countGates expr,
        numLiterals = countLiterals expr,
        depth = calculateDepth expr,
        fanout = calculateFanout expr
    }
    where
        countGates (Var _) = 0
        countGates (Not e) = 1 + countGates e
        countGates (And e1 e2) = 1 + countGates e1 + countGates e2
        countGates (Or e1 e2) = 1 + countGates e1 + countGates e2
        countGates (Xor e1 e2) = 1 + countGates e1 + countGates e2
        
        countLiterals (Var _) = 1
        countLiterals (Not e) = countLiterals e
        countLiterals (And e1 e2) = countLiterals e1 + countLiterals e2
        countLiterals (Or e1 e2) = countLiterals e1 + countLiterals e2
        countLiterals (Xor e1 e2) = countLiterals e1 + countLiterals e2
        
        calculateDepth (Var _) = 0
        calculateDepth (Not e) = 1 + calculateDepth e
        calculateDepth (And e1 e2) = 1 + max (calculateDepth e1) (calculateDepth e2)
        calculateDepth (Or e1 e2) = 1 + max (calculateDepth e1) (calculateDepth e2)
        calculateDepth (Xor e1 e2) = 1 + max (calculateDepth e1) (calculateDepth e2)
        
        calculateFanout expr = Map.fromListWith (+) (collectVars expr)
        collectVars (Var c) = [(c, 1)]
        collectVars (Not e) = collectVars e
        collectVars (And e1 e2) = collectVars e1 ++ collectVars e2
        collectVars (Or e1 e2) = collectVars e1 ++ collectVars e2
        collectVars (Xor e1 e2) = collectVars e1 ++ collectVars e2

-- Pretty print boolean expression
prettyPrint :: BoolExpr -> String
prettyPrint (Var c) = [c]
prettyPrint (Not e) = prettyPrint e ++ "'"
prettyPrint (And e1 e2) = "(" ++ prettyPrint e1 ++ " * " ++ prettyPrint e2 ++ ")"
prettyPrint (Or e1 e2) = "(" ++ prettyPrint e1 ++ " + " ++ prettyPrint e2 ++ ")"
prettyPrint (Xor e1 e2) = "(" ++ prettyPrint e1 ++ " ⊕ " ++ prettyPrint e2 ++ ")"

-- Test cases
testXOR :: BoolExpr
testXOR = Or (And (Var 'A') (Not (Var 'B'))) (And (Not (Var 'A')) (Var 'B'))

testFullAdder :: BoolExpr
testFullAdder = Xor (Xor (Var 'A') (Var 'B')) (Var 'C')

testMux :: BoolExpr
testMux = Or (And (Not (Var 'S')) (Var 'A')) (And (Var 'S') (Var 'B'))

-- Main execution
main :: IO ()
main = do
    putStrLn "Boolean Logic Circuit Minimizer - Haskell Implementation"
    putStrLn "=========================================================\n"
    
    putStrLn "Test 1: XOR Gate"
    putStrLn $ "Expression: " ++ prettyPrint testXOR
    putStrLn $ "Minimized: " ++ minimize testXOR ['A', 'B']
    let metrics1 = calculateComplexity testXOR
    putStrLn $ "Complexity: " ++ show metrics1 ++ "\n"
    
    putStrLn "Test 2: Full Adder Sum"
    putStrLn $ "Expression: " ++ prettyPrint testFullAdder
    putStrLn $ "Minimized: " ++ minimize testFullAdder ['A', 'B', 'C']
    let metrics2 = calculateComplexity testFullAdder
    putStrLn $ "Complexity: " ++ show metrics2 ++ "\n"
    
    putStrLn "Test 3: 2:1 Multiplexer"
    putStrLn $ "Expression: " ++ prettyPrint testMux
    putStrLn $ "Minimized: " ++ minimize testMux ['A', 'B', 'S']
    let metrics3 = calculateComplexity testMux
    putStrLn $ "Complexity: " ++ show metrics3