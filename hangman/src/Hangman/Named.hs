{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Hangman.Named
    ( Named
    , name
    , unName
    , pattern Named
    ) where

newtype Named name a = MkNamed a

unName :: Named name a -> a
unName (MkNamed x) = x

pattern Named :: a -> Named name a
pattern Named a <- MkNamed a

name :: a -> (forall name. Named name a -> r) -> r
name x f = f $ MkNamed x
