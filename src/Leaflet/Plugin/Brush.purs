module Leaflet.Plugin.Brush where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Ref (REF, Ref)
import DOM (DOM)
import Data.Maybe (Maybe)
import Leaflet.Core as LC

import Debug.Trace as DT

plugin
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM, ref ∷ REF|e) m
  ⇒ LC.Layer
  → LC.Leaflet
  → m (Ref (Maybe Unit))
plugin lay leaf =
  LC.onAddRemove onAdd onRemove lay leaf

onAdd ∷ ∀ e. LC.Layer → LC.Leaflet → Eff (dom ∷ DOM|e) Unit
onAdd lay leaf = do
  DT.traceAnyA "on add brush"
  pure unit

onRemove ∷ ∀ e. LC.Layer → LC.Leaflet → Maybe Unit → Eff (dom ∷ DOM|e) Unit
onRemove lay leaf mbState = do
  DT.traceAnyA "on remove brush"
  pure unit
