module Leaflet.Plugin.Heatmap (mkHeatmap) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (Ref, REF)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)

import Data.Array as A
import Data.Foldable (class Foldable, for_, intercalate)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple (fst, snd)

import DOM (DOM)
import DOM.Classy.Element (class IsElement, toElement, setAttribute)
import DOM.Classy.Node (appendChild)

import Leaflet.Types (Leaflet, Layer, Degrees, zoomToNumber, Bounds, mapToEvented)
import Leaflet.Plugin.Heatmap.Internal.Canvas as C
import Leaflet.Util ((∘), (×), type (×))

import Math as Math

import Unsafe.Coerce

onAddRemove = unsafeCoerce unit
testProp ∷ _ → _ (Maybe _)
testProp = unsafeCoerce unit
setStyle = unsafeCoerce unit
getSize = unsafeCoerce unit
any3d = unsafeCoerce unit
zoomAnimation = unsafeCoerce unit
getPanes = unsafeCoerce unit
containerPointToLayerPoint = unsafeCoerce unit
setPosition = unsafeCoerce unit
eventZoom = unsafeCoerce unit
eventCenter = unsafeCoerce unit
getZoomScale = unsafeCoerce unit
getCenterOffset = unsafeCoerce unit
getMapPanePos = unsafeCoerce unit
multiplyPoint = unsafeCoerce unit
subtractPoint = unsafeCoerce unit
setTransform = unsafeCoerce unit
on = unsafeCoerce unit
getMaxZoom = unsafeCoerce unit
getZoom = unsafeCoerce unit
addPoint = unsafeCoerce unit
latLngToContainerPoint = unsafeCoerce unit
contains = unsafeCoerce unit

mkHeatmap
  ∷ ∀ e m f
  . MonadEff (dom ∷ DOM, ref ∷ REF, canvas ∷ C.CANVAS|e) m
  ⇒ Foldable f
  ⇒ f { lat ∷ Degrees, lng ∷ Degrees, i ∷ Number }
  → Layer
  → Leaflet
  → m (Ref Unit)
mkHeatmap items opts lay leaf = do
  onAddRemove (onAdd opts items) onRemove lay leaf

onRemove ∷ ∀ e. Unit → Eff e Unit
onRemove _ = pure unit


onAdd
  ∷ ∀ e m f
  . Foldable f
  ⇒ C.HeatmapOptions
  → f { lat ∷ Degrees, lng ∷ Degrees, i ∷ Number }
  → Layer
  → Leaflet
  → Eff (dom ∷ DOM, canvas ∷ C.CANVAS|e) Unit
onAdd opts items lay leaf = do
  originProp ←
    testProp [ "transformOrigin", "WebkitTransformOrigin", "msTransformOrigin" ]

  canvas ← C.createCanvas
  let
    canvasEl = C.canvasToElement canvas

  for_ originProp \p →
    setStyle p "50% 50%" canvasEl

  x × y ← getSize leaf
  _ ← C.setCanvasWidth (Int.toNumber x) canvas
  _ ← C.setCanvasHeight (Int.toNumber y) canvas

  threeD ← any3d
  isZoom ← zoomAnimation leaf
  let
    animClass
      | threeD && isZoom = "leaflet-zoom-animated"
      | otherwise = "leaflet-zoom-hide"

  setAttribute "class"
    (intercalate " " [ "leaflet-ps-heatmap-layer", "leaflet-layer", animClass ])
    canvasEl

  panes ← getPanes leaf
  for_ (SM.lookup "overlayPane" panes) $ appendChild canvasEl

  let
    reset _ = do
      topLeft ← containerPointToLayerPoint (0 × 0) leaf
      mapSize ← getSize leaf
      _ ← C.setCanvasWidth (Int.toNumber x) canvas
      _ ← C.setCanvasHeight (Int.toNumber y) canvas
      setPosition canvasEl topLeft
      redraw canvas items opts leaf

    zoomAnim e = void $ runMaybeT do
      zoom ← MaybeT $ eventZoom e
      center ← MaybeT $ eventCenter e
      scale ← getZoomScale zoom leaf
      offset ← getCenterOffset center leaf
      panePos ← getMapPanePos leaf
      let coord = offset `multiplyPoint` (-1.0 * scale) # flip subtractPoint panePos
      setTransform canvasEl offset scale

  when (threeD && isZoom) $ mapToEvented leaf # on "zoomanim" zoomAnim

  mapToEvented leaf # on "moveend" reset

  redraw canvas items opts leaf

redraw
  ∷ ∀ e f
  . Foldable f
  ⇒ C.CanvasElement
  → f { lng ∷ Degrees, lat ∷ Degrees, i ∷ Number }
  → C.HeatmapOptions
  → Leaflet
  → Eff (dom ∷ DOM, canvas ∷ C.CANVAS|e) Unit
redraw el items opts leaf = do
  size ← getSize leaf
  maxZoom ← map zoomToNumber $ getMaxZoom leaf
  zoom ← map zoomToNumber $ getZoom leaf
  panePos ← getMapPanePos leaf

  let
    radius ∷ Int
    radius = Int.floor opts.radius

    bounds ∷ Bounds
    bounds = ((-radius) × (-radius)) × (addPoint (radius × radius) size)

    intensityMultiplier ∷ Number
    intensityMultiplier =
      1.0 / (Math.pow 2.0 $ Math.max 0.0 $ Math.min 12.0 $ maxZoom - zoom)

    cellSize ∷ Int
    cellSize = radius / 2

    offsetX ∷ Int
    offsetX = (fst panePos) `mod` cellSize

    offsetY ∷ Int
    offsetY = (snd panePos) `mod` cellSize

    alterFn r Nothing = Just r
    alterFn rr (Just r) =
      let newI = rr.i + r.i
          newX = (r.x * r.i + rr.x * rr.i) / newI
          newY = (r.y * r.i + rr.y * rr.i) / newI
      in Just { x: newX, y: newY, i: newI }

    foldFn
      ∷ ∀ e
      . Map.Map (Int × Int) { x ∷ Number, y ∷ Number, i ∷ Number }
      → { lat ∷ Degrees, lng ∷ Degrees, i ∷ Number }
      → Eff (dom ∷ DOM|e) (Map.Map (Int × Int) { x ∷ Number, y ∷ Number, i ∷ Number })
    foldFn acc { lat, lng, i} = do
      p@(px × py) ← latLngToContainerPoint {lat, lng} leaf
      if not $ contains bounds p
        then pure acc
        else do
        let
          gx = (px - offsetX) / cellSize + 2
          gy = (py - offsetY) / cellSize + 2
          item = { x: Int.toNumber px, y: Int.toNumber py, i: i * intensityMultiplier }
        pure $ Map.alter (alterFn item) (gx × gy) acc

    groupPoints
      ∷ ∀ e
      . Array { lat ∷ Degrees, lng ∷ Degrees, i ∷ Number }
      → Eff (dom ∷ DOM|e) (Array { x ∷ Number, y ∷ Number, i ∷ Number })
    groupPoints is =
      map (A.fromFoldable ∘ Map.values) $ A.foldRecM foldFn Map.empty is

    adjustPoints
      ∷ { x ∷ Number, y ∷ Number, i ∷ Number }
      → { x ∷ Number, y ∷ Number, i ∷ Number }
    adjustPoints {x, y, i} =
      { x: x - opts.radius / 2.0
      , y: y - opts.radius / 2.0
      , i: Math.min i opts.maxIntensity
      }

  grouppedPoints ← groupPoints $ A.fromFoldable items

  liftEff
    $ C.draw el opts
    $ map adjustPoints
    $ grouppedPoints
