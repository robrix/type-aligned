{-# LANGUAGE ConstraintKinds, ScopedTypeVariables #-}
module Data.TASequence.Internal
( instShow
, showsBinaryWith
, showsUnaryWith
, Forall2
) where

import Data.Constraint
import Data.Constraint.Forall
import Data.Functor.Classes (showsBinaryWith, showsUnaryWith)

type Forall2 c = ForallF (ForallF c)

instShow :: forall c x y . ForallF (ForallF Show) c :- Show (c x y)
instShow = Sub $ case (instF :: ForallF (ForallF Show) c :- ForallF Show (c x)) of
  Sub Dict -> case (instF :: ForallF Show (c x) :- Show (c x y)) of
    Sub Dict -> Dict
