import Control.Monad
import Control.Applicative

-- Please note, the following could have used Maybe instead of
-- my own defined 'Validity' datatype, but that would be
-- boring.

main = do
 putStrLn "Welcome to BracketsCheck, please input string: "
 string <- getLine
 let result = checkBrackets string
 putStrLn $ "Your string has " ++ (if result then "valid" else "invalid") ++ " brackets."

checkBrackets :: String -> Bool
checkBrackets s 
 = foldl (\x y -> x >>= bCount y) (Valid 0) s == Valid 0

bCount :: Char -> Int -> Validity Int
bCount c i
 | i < 0      = Invalid
 | c == '('   = Valid (i + 1)
 | c == ')'   = Valid (i - 1)
 | otherwise  = Valid i

-------------------------------------------------------------

data Validity a = Valid a | Invalid deriving (Show, Eq)

instance Functor Validity where
 fmap f Invalid    = Invalid
 fmap f (Valid x)  = Valid (f x)

instance Applicative Validity where
 pure x                     = Valid x
 (<*>) (Valid x) (Valid y)  = Valid (x y)
 (<*>) _ _                  = Invalid

instance Monad (Validity) where
 return x           = Valid x
 (>>=) (Valid x) f  = f x
 (>>=) _ _          = Invalid
