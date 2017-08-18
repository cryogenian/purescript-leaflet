module Leaflet.Core.Control
  ( layers
  , addTo
  , remove
  , control
  , initControl
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, writeRef, readRef)
import DOM (DOM)
import DOM.Classy.Element (class IsElement, toElement)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3, Fn4, runFn4, mkFn2)
import DOM.Node.Types (Element)
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Leaflet.Core.Types as T
import Leaflet.Util ((×), type (×))

import Debug.Trace as DT

foreign import layers_
  ∷ ∀ e r. Fn3 (SM.StrMap T.Layer) (SM.StrMap T.LayerGroup) r (Eff (dom ∷ DOM|e) T.Control)

foreign import addTo_
  ∷ ∀ e. Fn2 T.Leaflet T.Control (Eff (dom ∷ DOM|e) T.Control)

foreign import remove_
  ∷ ∀ e. T.Control → Eff (dom ∷ DOM|e) Unit

foreign import control_
  ∷ ∀ e. Eff e T.Control

foreign import initControl_
  ∷ ∀ e. Fn4
      (Fn2 T.Control T.Leaflet (Eff (dom ∷ DOM, ref ∷ REF|e) Element))
      (Fn2 T.Control T.Leaflet (Eff (dom ∷ DOM, ref ∷ REF|e) Unit))
      T.Control
      T.Leaflet
      (Eff (dom ∷ DOM, ref ∷ REF|e) Unit)

initControl
  ∷ ∀ e a m n
  . MonadEff (dom ∷ DOM, ref ∷ REF|e) m
  ⇒ IsElement n
  ⇒ (T.Control → T.Leaflet → Eff (dom ∷ DOM, ref ∷ REF|e) (a × n))
  → (T.Control → T.Leaflet → Maybe (a × n) → Eff (dom ∷ DOM, ref ∷ REF|e) Unit)
  → T.Control
  → T.Leaflet
  → m (Ref (Maybe (a × n)))
initControl init finish ctrl leaf = liftEff do
  ref ← newRef Nothing
  runFn4 initControl_
    (mkFn2 \c lf → do
        res × e ← init c lf
        writeRef ref $ Just $ res × e
        pure $ toElement e)
    (mkFn2 \c lf → do
        mbv ← readRef ref
        finish c lf mbv)
    ctrl
    leaf
  pure ref

control
  ∷ ∀ m e
  . MonadEff e m
  ⇒ m T.Control
control = liftEff control_

layers
  ∷ ∀ m e r1 r2
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 (T.LayerControlConf ())
  ⇒ SM.StrMap T.Layer
  → SM.StrMap T.LayerGroup
  → Record r1
  → m T.Control
layers bs os r =
  liftEff $ runFn3 layers_ bs os r

addTo
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → T.Control
  → m T.Control
addTo leaf c =
  liftEff $ runFn2 addTo_ leaf c

remove
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Control
  → m Unit
remove c =
  liftEff $ remove_ c
