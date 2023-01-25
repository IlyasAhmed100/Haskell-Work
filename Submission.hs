module Submission where
import Data.Char ()
import Data.Maybe ()
import Data.List()

-- Defining the BinOp datatype 
data BinOp = Plus | Minus | Times | Div
    deriving (Show, Eq, Enum)

-- Defining the Expr datatype
data Expr = Const Int | Op BinOp Expr Expr
    deriving (Eq, Show)

-- Question 1 

-- Computes price of pizza using simple maths and truncating the result at to 2dp
pizzaPricing :: Float -> Int -> Int -> Float -> Float
pizzaPricing diameterOfPizza numberOfTopping  numberOfSauce deliveryDistance = totalPrice
    where 
        areaOfPizza = pi * radiusOfPizza * radiusOfPizza
        radiusOfPizza = diameterOfPizza / 2
        pizzaToppingCost = fromIntegral numberOfTopping * areaOfPizza * 0.001
        pizzaBaseCost = areaOfPizza * 0.002
        pizzaSauceCost = fromIntegral numberOfSauce * 0.55
        deliveryCost = deliveryDistance * 0.9
        price = ((pizzaToppingCost + pizzaBaseCost + pizzaSauceCost) * 1.5) + deliveryCost 
        totalPrice = fromIntegral (truncate (price * 100)) / 100
    

-- Question 2 

{- Creates a new list of elements greater thanthe average of the previous list and 
gets the length of the new list -}
howManyAboveAverage :: [Double] -> Int
howManyAboveAverage x = length (filter (>= mean) x)  
    where 
        mean = sum x / fromIntegral (length x)

-- Question 3a

sampleExpr :: Expr
sampleExpr = Op Div (Op Plus (Const 4) (Op Times (Const 2) (Const 33))) (Const 2)

sampleExpr2 :: Expr 
sampleExpr2 = Op Times (Const 76) (Op Times (Const 92) (Const 66))

-- Defining the symbls for the binary operators
definingOps :: BinOp -> String
definingOps Plus = "+"
definingOps Minus = "-"
definingOps Times = "*"
definingOps Div = "/"

-- Splits the expression into operators and constants and appends tham as a string
expr2String :: Expr -> String
expr2String (Const int) = show int
expr2String (Op binOp expr1 expr2) =  expr2StringBracketing expr1 ++ 
                                      definingOps binOp ++ 
                                      expr2StringBracketing expr2
    where
        expr2StringBracketing :: Expr -> String -- Appends brackets where neccessary 
        expr2StringBracketing (Const int) = expr2String (Const int)
        expr2StringBracketing (Op bO e1 e2) = "(" ++ expr2String (Op bO e1 e2) ++ ")"

-- Question 3b

-- Input and ouput are the same
rewriteAssoc :: Expr -> Expr
rewriteAssoc expression = expression

-- Question 4

-- Defining the binary operators for addition and multiplication
definingOperators :: BinOp -> Int -> Int -> Int
definingOperators Plus a b = a + b
definingOperators Times a b = a * b

-- Checks if operation is allowed under countdown rules
isOperationLegal :: BinOp -> Int -> Int -> Bool
isOperationLegal Plus _ _ = True
isOperationLegal Times _ _ = True

{- Calculates the expression of the constants on the left and right o fthe operator
and checks if the expression is legal -}
calculateExpression :: Expr -> [Int]
calculateExpression (Const x) = [x | x > 0]
calculateExpression (Op op left right) = [definingOperators op x y 
                                                    | x <- calculateExpression left
                                                    , y <- calculateExpression right
                                                    , isOperationLegal op x y]

-- Gives all possible combinations of a list 
listOfLists :: [Int] -> [[Int]]
listOfLists [] = [[]]
listOfLists (x:xs) = yss ++ map (x:) yss
              where yss = listOfLists xs

-- Adds an element to every index of a list recursively 
insertElementInLists :: Int -> [Int] -> [[Int]]
insertElementInLists x [] = [[x]]
insertElementInLists x (y:ys) = (x:y:ys) : map (y:) (insertElementInLists x ys)

-- Shows different ways of ordering the elements in the list
differentOrderingOfList :: [Int] -> [[Int]]
differentOrderingOfList [] = [[]]
differentOrderingOfList xs = foldr (concatMap . insertElementInLists) [[]] xs

{- Concatenates the 'insertElementInLists' and the 'differentOrderingOfList' functions
giving all the possible ways of choosing elements from the list in any legal order -}
listCombination :: [Int] -> [[Int]]
listCombination = concatMap differentOrderingOfList . listOfLists

-- Combines the two expressins using the Plus and Times operators respectively
linkTogether :: Expr -> Expr -> [Expr]
linkTogether left right = [Op op left right | op <- [Plus, Times]]

-- Calculates every expression that can be made from the list of integers
allExpressions :: [Int] -> [Expr]
allExpressions [] = []
allExpressions [x] = [Const x]
allExpressions xs = [e | (leftExpr, rightExpr) <- allSplits xs
                        , left <- allExpressions leftExpr
                        , right <- allExpressions rightExpr
                        , e <- linkTogether left right]

-- Computes the expressions that satisfy the countdown problem and are legal
countdownAllSolutions :: [Int] -> Int -> [Expr]
countdownAllSolutions xs x = [e | xs' <- listCombination xs
                                , e <- allExpressions xs'
                                , calculateExpression e == [x]]

-- Shows all possible combinations of splitting up a list
allSplits :: [Int] -> [([Int],[Int])]
allSplits [] = []
allSplits [_] = []
allSplits (z:zs) = ([z], zs) : [(z: leftExpr, rightExpr)
                    | (leftExpr, rightExpr) <- allSplits zs]