import Control.Monad
import Data.Maybe
import Control.Applicative

-- The below demonstrates a list comprehension of [x | x <- [1..50], elem '7' (show x)] under the bonnet.

sevensOnly :: [Int]
sevensOnly = [1..50] >>= \x -> guard ('7' `elem` (show x)) >> return x

-- EXPLANATION: Binds 1..50 to a function that either succeeds or fails, if it succeds, we return the number
-- with >>, if it fails, then it'll return the identity '[]' and mappend that (basically do nothing and go on
-- to the next number). IT IS IMPORTANT that lists are instances of MonadPlus, because when we mappend the
-- empty list for failed guards, it has no change to the result (we just append []).
