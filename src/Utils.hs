
module Utils (
    removePrefix
  , removeSuffix
  , split
  , liftExceptT
  , allEq
) where

import Control.Monad.Trans.Except

removePrefix :: Eq a => [a] -> [a] -> [a]
removePrefix [] [] = []
removePrefix [] s  = s
removePrefix (p:ps) (c:cs)
  | p == c    = removePrefix ps cs
  | otherwise = error "Invalid prefix"
removePrefix _ _ = error "Invalid prefix"

removeSuffix :: Eq a => [a] -> [a] -> [a]
removeSuffix suf = reverse . removePrefix (reverse suf) . reverse

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split c cs 
  | null (dropWhile (/= c) cs) = [cs]
  | otherwise = taken : split c (tail rest)
      where (taken, rest) = span (/= c) cs

liftExceptT :: Monad m => Except a b -> ExceptT a m b
liftExceptT = ExceptT . return . runExcept

allEq :: Eq a => [a] -> Bool
allEq []     = True
allEq (x:xs) = all (== x) xs