
{-# LANGUAGE PackageImports , UnicodeSyntax #-} 

module Nat where

data Nat = Z
         | S Nat
         deriving (Show, Eq, Ord, Read)

instance Enum Nat where
        succ = S
        pred Z = Z
        pred (S n) = n
        toEnum n | n <= 0 = 0
                 | otherwise = S $ toEnum $ n - 1
        fromEnum Z = 0
        fromEnum (S n) = 1 + fromEnum n

fromNat :: Nat → Int
fromNat = fromEnum
toNat :: Int → Nat
toNat = toEnum

instance Num Nat where
        Z + n = n
        (S n) + m = S (n + m)
        n - Z = n
        Z - _ = Z
        (S n) - (S m) = n - m
        Z * _ = Z
        _ * Z = Z
        n * (S m) = n + n * m
        abs = id
        negate = id
        signum Z = Z
        signum (S _) = S Z
        fromInteger n | n <= 0 = Z
                      | otherwise = S $ fromInteger $ n - 1
