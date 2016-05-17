
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
data Maybe a = Just a | Nothing
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
palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse' xs
                where
                  reverse' []     = []
                  reverse' (x:xs) = (reverse' xs) ++ [x]


{- 3.5 -}
intersperse :: a -> [[a]] -> [a]
intersperse _ []      = []
intersperse c (x:xs)  = x ++ c:(intersperse c xs)


{- 3.6 -}

-- sorts a list of lists by the length of each sublist
sortByLength :: [[a]] -> [[a]]
sortByLength x = sortBy order x
  where order a b =
          let
            lenA = length a
            lenB = length b
          in lenA `compare` lenB
