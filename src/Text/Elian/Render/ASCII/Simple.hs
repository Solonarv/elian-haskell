{-# LANGUAGE
    OverloadedStrings
    #-}
module Text.Elian.Render.ASCII.Simple (
    renderSimpleAscii,
    module Text.PrettyPrint.Boxes
    ) where

import Text.Elian.Types

import Data.List
import Text.PrettyPrint.Boxes

renderSimpleAscii :: Applicative m => ElianRendererM m Box
renderSimpleAscii = ElianRenderer $ pure . vsep 5 top. map (hsep 2 top . map renderCharacter)

-- | render a character to a 5x7 box.
renderCharacter :: ElianChar -> Box
renderCharacter (Alphabetic pos seg) = case seg of
    Plain -> case pos of
        PositionBottomLeft  -> vcat left [s5, s5, "    |", "    |", "----+", s5, s5]
        PositionLeft        -> vcat left [s5, s5, "----+", "    |", "----+", s5, s5]
        PositionTopLeft     -> vcat left [s5, s5, "    |", "    |", "----+", s5, s5]
        PositionBottom      -> vcat left [s5, s5, "+---+", "|   |", "|   |", s5, s5]
        PositionCenter      -> vcat left [s5, s5, "+---+", "|   |", "+---+", s5, s5]
        PositionTop         -> vcat left [s5, s5, "|   |", "|   |", "+---+", s5, s5]
        PositionBottomRight -> vcat left [s5, s5, "+----", "|    ", "|    ", s5, s5]
        PositionRight       -> vcat left [s5, s5, "+----", "|    ", "+----", s5, s5]
        PositionTopRight    -> vcat left [s5, s5, "|    ", "|    ", "+----", s5, s5]
    Tailed -> case pos of
        PositionBottomLeft  -> vcat left [     s5,      s5, "----+", "    |", "    |", "    |", "    |"]
        PositionLeft        -> vcat left [     s5,      s5, "----+", "    |", "    |", "    |", "----+"]
        PositionTopLeft     -> vcat left ["    |", "    |", "    |", "    |", "----+",      s5,      s5]
        PositionBottom      -> vcat left [     s5,      s5, "+---+", "|   |", "|   |", "    |", "    |"]
        PositionCenter      -> vcat left [     s5,      s5, "+---+", "|   |", "+---+", "    |", "    |"]
        PositionTop         -> vcat left ["    |", "    |", "|   |", "|   |", "+---+",      s5,      s5]
        PositionBottomRight -> vcat left [     s5,      s5, "+----", "|    ", "|    ", "|    ", "|    "]
        PositionRight       -> vcat left [     s5,      s5, "+----", "|    ", "|    ", "|    ", "+----"]
        PositionTopRight    -> vcat left ["|    ", "|    ", "|    ", "|    ", "+----",      s5,      s5]
    TailDotted -> case pos of
        PositionBottomLeft  -> vcat left [     s5, " --- ", "----+", "    |", "    |", "    |", "    |"]
        PositionLeft        -> vcat left [     s5, " --- ", "----+", "    |", "    |", "    |", "----+"]
        PositionTopLeft     -> vcat left ["    |", "    |", " |  |", " |  |", "----+",      s5,      s5]
        PositionBottom      -> vcat left [     s5, " --- ", "+---+", "|   |", "|   |", "    |", "    |"]
        PositionCenter      -> vcat left [     s5, " --- ", "+---+", "|   |", "+---+", "    |", "    |"]
        PositionTop         -> vcat left ["    |", "    |", "|   |", "|   |", "+---+", " --- ",      s5]
        PositionBottomRight -> vcat left [     s5, " --- ", "+----", "|    ", "|    ", "|    ", "|    "]
        PositionRight       -> vcat left [     s5, " --- ", "+----", "|    ", "|    ", "|    ", "+----"]
        PositionTopRight    -> vcat left ["|    ", "|    ", "|  | ", "|  | ", "+----",      s5,      s5]
renderCharacter (Other c) = vcat left [s5, s5, s5, text ("  " ++ c:"  "), s5, s5, s5]

-- | 5 spaces
s5 :: Box
s5 = text "     "