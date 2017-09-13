{-# LANGUAGE
    RecordWildCards
    #-}
module Text.Elian.Render.Rasterific.Simple (
    -- * Renderers
    RenderConfig(..), def,
    renderImageSimple,
    renderCharacter,
    -- * Pixel types, reexported for convenience
    PixelRGB8(..), PixelRGBA8(..),
    -- * Rasterific functions reexported for convenience
    renderDrawing, 
    ) where

import Text.Elian.Types

import Graphics.Text.TrueType (Font, loadFontFile)

import Codec.Picture.Types (PixelRGBA8(..), PixelRGB8(..))

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations (translate)

import Data.Maybe
import Data.Default
import Control.Monad

-- | Configuration options for the renderer.
data RenderConfig = RenderConfig {
    rcBoxSize         :: !Float,  -- ^ the size of the central box
    rcVerticalSpace   :: !Float, -- ^ the amount of space reserved for "decoration", i.e. lengthened lines and/or dashes
    rcLineThickness   :: !Float, -- ^ how thick the lines making up the characters should be
    rcCharacterBuffer :: !Float, -- ^ how much buffer distance should be between characters
    rcVerticalWords   :: !Bool, -- ^ whether letters in a word should be arranged vertically (True) or horizontally (False)
    rcWordBuffer      :: !Float, -- ^ how much buffer distance should be between words
    rcVerticalFlow    :: !Bool, -- ^ whether overall text flow should be vertical-first (True) or horizontal-first (False)
    rcFallbackFont    :: !(Maybe Font) -- ^ Font to use for rendering non-Elian characters. 
    }

{- | Default configuration:
     > RenderConfig {
     >     rcBoxSize = 15,
     >     rcVerticalSpace = 8,
     >     rcLineThickness = 2,
     >     rcCharacterBuffer = 8,
     >     rcVerticalWords = True,
     >     rcWordBuffer = 15,
     >     rcVerticalFlow = False,
     >     rcFallbackFont = Nothing
     >     }
-}
instance Default RenderConfig where
    def = RenderConfig {
        rcBoxSize = 15,
        rcVerticalSpace = 8,
        rcLineThickness = 2,
        rcCharacterBuffer = 8,
        rcVerticalWords = True,
        rcWordBuffer = 15,
        rcVerticalFlow = False,
        rcFallbackFont = Nothing
        }

-- | Renderer that produces a simple, boxy character representation.
renderImageSimple :: ElianRendererM ((->) RenderConfig) (Drawing px ())
renderImageSimple = ElianRenderer $ \ws cfg@RenderConfig{..} -> let
    buf = if rcVerticalFlow
        then V2 0 (rcWordBuffer + 2*rcVerticalSpace + rcBoxSize)
        else V2 (rcWordBuffer + rcBoxSize) 0
    in forM_ (zip [0..] ws) $ \(wn, w) ->
        withTransformation (translate (buf * V2 wn wn)) $
            renderWord cfg w
    

renderWord :: RenderConfig
           -> [ElianChar] -- ^ the characters making up the word to draw
           -> Drawing px ()
renderWord cfg@RenderConfig{..} cs = let
    buf = if rcVerticalWords    
        then V2 0 (rcCharacterBuffer + 2*rcVerticalSpace + rcBoxSize)
        else V2 (rcCharacterBuffer + rcBoxSize) 0
    in forM_ (zip [0..] cs) $ \(cn, c) ->
        withTransformation (translate (buf * V2 cn cn)) $
            renderCharacter cfg c

-- | Draw a single character, in "boxy" fashion
renderCharacter :: RenderConfig
                -> ElianChar -- ^ the character to draw
                -> Drawing px ()
renderCharacter RenderConfig{..} (Alphabetic pos seg) = case seg of
    Plain  -> stroke rcLineThickness (JoinMiter 0) (cap, cap) $ drawPositionBox rcBoxSize pos 
    Tailed -> stroke rcLineThickness (JoinMiter 0) (cap, cap) $ drawPositionBoxTailed rcBoxSize rcVerticalSpace pos
  where cap = CapStraight (rcLineThickness/2)
renderCharacter RenderConfig{..} (Other c) = case rcFallbackFont of
    Nothing -> return ()
    Just font -> printTextAt font (PointSize (rcBoxSize * 0.8)) (V2 (rcBoxSize/2) (rcBoxSize/2)) [c]

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
    | otherwise -- this is only PositionTopRight, so it's fine to hardcode it
        = [Line (V2 0 boxSize) (V2 boxSize boxSize), Line (V2 boxSize (-vSpace)) (V2 boxSize boxSize)]
    -- | otherwise = error ("can't happen " ++ show pos)
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