{-# LANGUAGE ScopedTypeVariables #-}
module Data.TASequence.Internal
( instShow
) where

import Data.Constraint
import Data.Constraint.Forall

instShow :: forall graph x y . ForallF (ForallF Show) graph :- Show (graph x y)
instShow = Sub $ case (instF :: ForallF (ForallF Show) graph :- ForallF Show (graph x)) of
  Sub Dict -> case (instF :: ForallF Show (graph x) :- Show (graph x y)) of
    Sub Dict -> Dict
