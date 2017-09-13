{-# LANGUAGE
    RecordWildCards
    #-}
module Text.Elian.Render.Rasterific.Simple where

import Text.Elian.Types

import Graphics.Rasterific
import Graphics.Rasterific.Texture

import Data.Maybe
import Control.Monad

data RenderConfig = RenderConfig {
    rcBoxSize :: Float,  -- ^ the size of the central box
    rcVerticalSpace :: Float, -- ^ the amount of space reserved for "decoration", i.e. lengthened lines and/or dashes
    rcLineThickness :: Float
    }

renderImageSimple :: ElianRendererM ((->) RenderConfig) (Drawing px ())
renderImageSimple = ElianRenderer $ \ws -> error "not implemented"

-- | Draw a single character, in "boxy" fashion
renderCharacter :: RenderConfig
                -> ElianChar -- ^ the character to draw
                -> Drawing px ()
renderCharacter RenderConfig{..} (Alphabetic pos seg) = case seg of
    Plain  -> stroke rcLineThickness (JoinMiter 0) (cap, cap) $ drawPositionBox rcBoxSize pos 
    Tailed -> stroke rcLineThickness (JoinMiter 0) (cap, cap) $ drawPositionBoxTailed rcBoxSize rcVerticalSpace pos
  where cap = CapStraight (rcLineThickness/2)

-- | Draw the central box for a character
drawPositionBox :: Float -- ^ the size of the box
                -> Position -- ^ which box-shape to draw
                -> [Line]
drawPositionBox boxSize pos = catMaybes $ [
        if hasRightEdge pos then Just $ Line (V2 boxSize 0) (V2 boxSize boxSize) else Nothing,
        if hasBottomEdge pos then Just $ Line (V2 0 boxSize) (V2 boxSize boxSize) else Nothing,
        if hasLeftEdge pos then Just $ Line (V2 0 0) (V2 0 boxSize) else Nothing,
        if hasTopEdge pos then Just $ Line (V2 0 0) (V2 boxSize 0) else Nothing
    ]

drawPositionBoxTailed :: Float -- ^ the size of the box
                      -> Float -- ^ the amound of space for the tail
                      -> Position -- ^ which box-shape to draw
                      -> [Line]
drawPositionBoxTailed boxSize vSpace pos
    | pos == PositionCenter
        = lineFromPath [V2 boxSize boxSize, V2 0 boxSize, V2 0 0, V2 boxSize 0, V2 boxSize (boxSize + vSpace)]
    | shouldGoDown && hasRightEdge pos
        = catMaybes $ [
            if hasLeftEdge pos then Just $ Line (V2 0 0) (V2 0 boxSize) else Nothing,
            if hasTopEdge pos then Just $ Line (V2 0 0) (V2 boxSize 0) else Nothing,
            Just $ Line (V2 boxSize 0) (V2 boxSize (boxSize + vSpace)),
            if hasBottomEdge pos then Just $ Line (V2 0 (boxSize + vSpace)) (V2 boxSize (boxSize + vSpace)) else Nothing]
    | shouldGoDown && hasLeftEdge pos
        = catMaybes $ [
            if hasLeftEdge pos then Just $ Line (V2 0 0) (V2 0 boxSize) else Nothing,
            if hasTopEdge pos then Just $ Line (V2 0 0) (V2 boxSize 0) else Nothing,
            if hasBottomEdge pos then Just $ Line (V2 0 (boxSize + vSpace)) (V2 boxSize (boxSize + vSpace)) else Nothing]
    | not shouldGoDown && hasRightEdge pos
        = catMaybes $ [
            if hasLeftEdge pos then Just $ Line (V2 0 0) (V2 0 boxSize) else Nothing,
            Just $ Line (V2 boxSize (-vSpace)) (V2 boxSize boxSize ),
            if hasBottomEdge pos then Just $ Line (V2 0 boxSize) (V2 boxSize boxSize) else Nothing]
    | not shouldGoDown && hasLeftEdge pos -- this is only PositionTopRight, so it's fine to hardcode it
        = [Line (V2 0 boxSize) (V2 boxSize boxSize), Line (V2 boxSize (-vSpace)) (V2 boxSize boxSize)]
    | otherwise = error "can't happen"
  where
    shouldGoDown = hasTopEdge pos || not (hasBottomEdge pos)

positionDash :: Float -- ^ the size of the box
             -> Float -- ^ the amount of space for decorations
             -> Position -- ^ which box-shape the dash is for
             -> Line
positionDash boxSize vSpace pos
    | hasTopEdge pos
        = Line (V2 (vSpace*0.2) (-vSpace/3)) (V2 (vSpace*0.8) (vSpace/3))
    | otherwise
        = Line (V2 (boxSize/2) 0) (V2 (boxSize/2) (boxSize/2))