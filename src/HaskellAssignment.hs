module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found

-- making a helper function which will keep track of the index with default value 0
findFirst needle haystack = helper needle haystack 0 where
  -- if list empty, return NoMatch
  helper _ [] _ = NoMatch
  helper needle (hay:stack) index =
    -- if needle is true for the current element hay
    -- return a Match with the current index
    if needle hay
      then Match index
      -- otherwise, keep calling itself
      -- with the remaining elements in the list
      -- and increment index
      else helper needle stack (index + 1)

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