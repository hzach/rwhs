
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

cross :: Point -> Point -> Double
cross (Point x1 y1) (Point x2 y2) = x1*y2 - x2*y1


-- Computes the driection of the turn when traversing from point a to b to c in R^2. In other words,
-- this function returns the sign of the angle between the line segments ab and bc
computeTurn :: Point -> Point -> Point -> Direction
computeTurn (Point a1 a2) (Point b1 b2) (Point c1 c2)
             | v `cross` u > 0  = LeftTurn
             | v `cross` u == 0 = Straight
             | v `cross` u < 0  = RightTurn
            where  -- Computes the z component of the cross product between two vectors
              v = Point (b1 - a1) (b2 - a2)
              u = Point (c1 - b1) (c2 - b2)


-- Sorts a list of points according to their distance from the origin in R^2
cartesianSort :: [Point] -> [Point]
cartesianSort ps = sortBy y (sortBy x ps)
  where
    y (Point _ y1) (Point _ y2) = y1 `compare` y2
    x (Point x1 _) (Point x2 _) = x1 `compare` x2


-- Sorts a list of angles
angleSort :: Point -> [Point] -> [Point]
angleSort p ps = sortBy (f p) ps
  where
    f p q r = case computeTurn p q r of
      LeftTurn  -> LT
      Straight  -> EQ
      RightTurn -> GT


grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan ps =
  let sorted  = cartesianSort ps
      p0      = head sorted
      sorted' = angleSort p0 (tail sorted)
  in p0:(computeHull sorted')


-- Computes the list of turns necessary to traverse a list of ordered points in R^2
computeHull :: [Point] -> [Point]
computeHull (x:y:z:zs) =
  case computeTurn x y z of
    RightTurn ->   x:z:(computeHull zs)
    otherwise -> x:y:z:(computeHull zs)
computeHull _ = []


-- grahamScan test:
points = [  Point 2 1
      , Point 5 2
      , Point 4 3
      , Point 5 4
      , Point 3 5
      , Point 3 4
      , Point 2 3  ]

main = print $ grahamScan points
