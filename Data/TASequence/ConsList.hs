{-# LANGUAGE FlexibleContexts, GADTs, PolyKinds, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.ConsList
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type aligned sequence, a head-tail list, with worst case constant time: '<|', and 'tviewl'.
--
-----------------------------------------------------------------------------
module Data.TASequence.ConsList(module Data.TASequence,ConsList(..)) where
import Control.Category
import Data.TASequence
import Data.TASequence.Internal

data ConsList c x y where
  CNil :: ConsList c x x
  Cons :: c x y -> ConsList c y z -> ConsList c x z

instance TASequence ConsList where
  tempty = CNil
  tsingleton c = Cons c CNil
  (<|) = Cons
  tviewl CNil = TAEmptyL
  tviewl (Cons h t) = h :< t

instance Category (ConsList c) where
  id = tempty
  (.) = flip (><)


instance Forall2 Show c => Show (ConsList c a b) where
  showsPrec _ CNil                   = showString "CNil"
  showsPrec d (Cons (x :: c x y) xs) = showsBinaryWith showsPrec showsPrec "Cons" d x xs
    \\ instShow @c @x @y
