{-# LANGUAGE BangPatterns, DeriveAnyClass, DeriveGeneric, FlexibleInstances,
             FunctionalDependencies, MultiParamTypeClasses, RankNTypes,
             TemplateHaskell #-}

module J.Logging (
  Log(..), LogLevel(..), Products(..), levelledLog,
  HasLog(..)
  ) where

import Prelude hiding((.), id)

import Control.Category(Category(..))
import Control.Lens(Profunctor(..), makeClassy)

newtype Log lv l a = Log (lv -> l -> a)

makeClassy 'Log

data LogLevel = D | I | E
  deriving Show

class Category p => Products p where
  first :: p a b -> p (a,c) (b,c)
  second :: p a b -> p (c,a) (c,b)
  (***) :: p a b -> p a' b' -> p (a,a') (b,b')

instance Products (->) where
  first f (a, c) = (f a, c)
  second f (c, a) = (c, f a)
  (f *** g) (a, a') = (f a, g a')

instance Category (Log lv) where
  (Log l1) . (Log l2) = Log ((.) <$> l1 <*> l2)
  id = Log (const id)

instance Profunctor (Log lv) where
  dimap g f (Log l) = Log (\lv -> f . l lv . g)

instance Functor (Log lv l) where
  fmap = dimap id

levelledLog :: LogLevel -> String -> String
levelledLog lv l = "[" ++ show lv ++ "] " ++ l
