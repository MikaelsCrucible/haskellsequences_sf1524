module Sequences where
import Data.Char (ord, chr)
-- | Returns the first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 :: Int -> Int -> Int
maxOf2 x y
  | x > y = x
  | otherwise = y
  
-- | Returns the largest of three Ints
maxOf3 :: Int -> Int -> Int -> Int
maxOf3 x y z = if x >= maxOf2 y z then
  x
  else
  maxOf2 y z
-- | Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit :: Char -> Bool
isADigit x
  | x >= '0' && x <= '9' = True
  | otherwise = False
-- | Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha :: Char -> Bool
isAlpha x
  | x >= 'a' && x <= 'z' = True
  | x >= 'A' && x <= 'Z' = True
  | otherwise = False
-- | Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt :: Char -> Int
digitToInt x = ord x - ord '0'
-- Pre: the character is one of '0'..'9'

-- | Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper :: Char -> Char
toUpper x
  | x >= 'a' && x <= 'z' = chr( ord x - ( ord 'a' - ord 'A' ) )
  | otherwise = x

--
-- Sequences and series
--

-- | Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n = a + d * fromIntegral n

-- | Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n
  | n == 0 = a
  | otherwise = r * geometricSeq a r ( n - 1 )

-- | Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n = (a * 2.0 + d * fromIntegral n) * fromIntegral ( n + 1 ) / 2.0

-- | Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n
  | r == 1.0 = a * fromIntegral( n + 1 )
  | otherwise = a * (1.0 - r ^ fromIntegral( n + 1 )) / (1.0 - r )
