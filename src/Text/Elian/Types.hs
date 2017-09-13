{-# LANGUAGE
    FlexibleInstances
    #-}
module Text.Elian.Types where

import Data.Functor.Identity
import Data.Char
import Data.String

-- | The type of Elian Script characters: a letter, represented by
--   a box-shape; or some other character, represented by itself
data ElianChar = Alphabetic Position Segment | Other Char deriving Eq

-- | Convert a Char into an Elian Script character.
mkElianChar :: Char -> ElianChar
mkElianChar c
    | isAsciiUpper c = aindexToElianChar (fromEnum c - upperA)
    | isAsciiLower c = aindexToElianChar (fromEnum c - lowerA)
    | otherwise      = Other c
  where
    upperA = fromEnum 'A'
    lowerA = fromEnum 'a'

-- | Convert a letter (represented by its position in the alphabet)
--   to an ElianChar. This function will @error@ if its argument is
--   not in the 0..26 range
aindexToElianChar :: Int -> ElianChar
aindexToElianChar i
    | i >=0 && i < 26 = Alphabetic (toEnum $ i `mod` 9) (toEnum $ i `div` 9)
    | otherwise       = error "aindexToElianChar: out of range"

instance Show ElianChar where
    show (Alphabetic pos seg) = show seg ++ ' ':drop 8 (show pos)
    show (Other c) = "Other " ++ show c

-- | The enum of cells in a 3x3 square, in the order used by the Elian Script:
-- > 2 | 5 | 8
-- > --+---+--
-- > 1 | 4 | 7
-- > --+---+--
-- > 0 | 3 | 6
data Position = PositionBottomLeft | PositionLeft | PositionTopLeft | PositionBottom | PositionCenter | PositionTop | PositionBottomRight | PositionRight | PositionTopRight
    deriving (Eq, Enum, Bounded, Show)

hasLeftEdge :: Position -> Bool
hasLeftEdge = (< 3) . fromEnum

hasRightEdge :: Position -> Bool
hasRightEdge = (>= 6) . fromEnum

hasTopEdge :: Position -> Bool
hasTopEdge = (==2) . (`mod` 3) . fromEnum

hasBottomEdge :: Position -> Bool
hasBottomEdge = (==0) . (`mod` 3) . fromEnum

-- | The enum of the three Elian Script squares
data Segment = Plain | Tailed | TailDotted
    deriving (Eq, Show, Enum, Bounded)
    
newtype ElianRendererM m a = ElianRenderer { renderElianViaM :: [[ElianChar]] -> m a}
type ElianRenderer = ElianRendererM Identity

renderElianVia :: ElianRenderer a -> [[ElianChar]] -> a
renderElianVia rend = runIdentity . renderElianViaM rend

strToElian :: String -> [[ElianChar]]
strToElian = map (map mkElianChar) . words