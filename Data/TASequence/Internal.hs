{-# LANGUAGE ConstraintKinds, ScopedTypeVariables #-}
module Data.TASequence.Internal
( instShow
, showsBinaryWith
, showsQuaternaryWith
, showsTernaryWith
, showsUnaryWith
, Forall2
, (\\)
) where

import Data.Constraint
import Data.Constraint.Forall
import Data.Functor.Classes (showsBinaryWith, showsUnaryWith)

type Forall2 c = ForallF (ForallF c)

instShow :: forall c x y . ForallF (ForallF Show) c :- Show (c x y)
instShow = Sub $ case (instF :: ForallF (ForallF Show) c :- ForallF Show (c x)) of
  Sub Dict -> case (instF :: ForallF Show (c x) :- Show (c x y)) of
    Sub Dict -> Dict

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith showsPrecA showsPrecB showsPrecC s d a b c = showParen (d > 10)
  $ showString s
  . showChar ' ' . showsPrecA 11 a
  . showChar ' ' . showsPrecB 11 b
  . showChar ' ' . showsPrecC 11 c

showsQuaternaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> (Int -> d -> ShowS) -> String -> Int -> a -> b -> c -> d -> ShowS
showsQuaternaryWith showsPrecA showsPrecB showsPrecC showsPrecD s n a b c d = showParen (n > 10)
  $ showString s
  . showChar ' ' . showsPrecA 11 a
  . showChar ' ' . showsPrecB 11 b
  . showChar ' ' . showsPrecC 11 c
  . showChar ' ' . showsPrecD 11 d
