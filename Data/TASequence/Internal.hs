{-# LANGUAGE ScopedTypeVariables #-}
module Data.TASequence.Internal
( instShow
, showsBinaryWith
, showsUnaryWith
) where

import Data.Constraint
import Data.Constraint.Forall
import Data.Functor.Classes (showsBinaryWith, showsUnaryWith)

instShow :: forall c x y . ForallF (ForallF Show) c :- Show (c x y)
instShow = Sub $ case (instF :: ForallF (ForallF Show) c :- ForallF Show (c x)) of
  Sub Dict -> case (instF :: ForallF Show (c x) :- Show (c x y)) of
    Sub Dict -> Dict
