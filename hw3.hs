
{- Below are the excerises from Real Worlf Haskell Chapter 3-}

import Data.List (sortBy)

{- 3.a: Recursive list type -}

-- Custom List type
data List a = Cons a (List a)
              | Nil
              deriving Show

-- Converts a Haskell list to the custom List
toList :: [a] -> List a
toList (x:xs) = Cons x (toList xs)
toList []     = Nil


-- Converse of toList
fromList :: List a -> [a]
fromList (Cons x (xs)) = x:(fromList xs)
fromList Nil           = []


{- 3.b: Single parameter Tree type with Maybe value -}
data Maybe a = Just a | Empty
data  Tree a = Node (Tree a) (Tree a) | Maybe a


{- 3.1, 3.2 -}
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs


{- 3.3 -}
mean :: (Fractional a) => [a] -> a
mean xs = (1/n) * sum' xs
  where
    n = fromIntegral $ length xs
    sum' (y:ys) = y + (sum' ys)
    sum' []     = 0


{- 3.4 -}
palindrome :: Ord a => [a] -> [a]
palindrome xs = xs ++ reverse' xs
  where
    reverse' []     = []
    reverse' (x:xs) = (reverse' xs) ++ [x]


{- 3.5 -}
isPalindrome :: Ord a => [a] -> Bool
isPalindrome []     = True
isPalindrome (x:[]) = False
isPalindrome (x:xs) =
  let end    = last xs
      len    = length xs
      middle = drop len xs
  in x == end && isPalindrome middle


{- 3.6 -}
intersperse :: a -> [[a]] -> [a]
intersperse _ []      = []
intersperse c (x:xs)  = x ++ c:(intersperse c xs)


{- 3.7 -}

-- sorts a list of lists by the length of each sublist
sortByLength :: [[a]] -> [[a]]
sortByLength x = sortBy order x
  where order a b =
          let
            lenA = length a
            lenB = length b
          in lenA `compare` lenB


{- 3.8 -}
height :: Tree a -> Int
height (Maybe _ )         = 0
height (Node left right)  = 1 + max (height left) (height right)


{- 3.9 -}
data Direction = LeftTurn | RightTurn | Straight deriving Show


{- 3.10 -}
data Point = Point Double Double deriving Show


-- Computes the driection of the turn when traversing from point a to b to c in R^2. In other words,
-- this function returns the sign of the angle between the line segments ab and bc
computeTurn :: Point -> Point -> Point -> Direction
computeTurn (Point a1 a2) (Point b1 b2) (Point c1 c2)
             | z_comp v u > 0  = LeftTurn
             | z_comp v u == 0 = Straight
             | z_comp v u < 0  = RightTurn
            where
              z_comp (Point x1 y1) (Point x2 y2) = x1*y2 - x2*y1 -- Computes the z component of the cross product between two vectors
              v = Point (b1 - a1) (b2 - a2)
              u = Point (c1 - b1) (c2 - b2)


computeTurns :: [Point] -> [Direction]
computeTurns (x:y:z:zs) = computeTurn x y z : computeTurns y:z:zs
computeTurns _ = []
