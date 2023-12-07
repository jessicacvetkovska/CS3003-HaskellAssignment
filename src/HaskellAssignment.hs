module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool

-- an empty string is a palindrome!
palindrome [] = True

-- reverse candidate will reverse the list
-- candidate == reverse candidate will check if the original
-- list is equal to the reversed list, which indicates that
-- it is a palindrome
palindrome candidate = candidate == reverse candidate