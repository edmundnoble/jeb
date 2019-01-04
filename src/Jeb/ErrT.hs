{-# language DerivingVia #-}
{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}

module Jeb.ErrT where

import Control.Applicative(liftA2)
import Control.Monad.Fail
import Control.Monad.Trans.Class(MonadTrans)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class(MonadIO)
import Data.Bifunctor(first)
import Data.Coerce(coerce)
import Data.Function(on)
import Data.Functor(($>))
import Text.PrettyPrint.ANSI.Leijen(Doc)

import qualified Text.PrettyPrint.ANSI.Leijen as PP

newtype ErrT f a = ErrT { runErrT :: f (Either PP.Doc a) }
        deriving Functor via ExceptT Doc f
        deriving Applicative via ExceptT Doc f
        deriving Monad via ExceptT Doc f
        deriving MonadTrans via ExceptT Doc
        deriving MonadIO via ExceptT Doc f

instance (Semigroup a, Applicative f) => Semigroup (ErrT f a) where
        (ErrT ea) <> (ErrT ea') = ErrT $
                combineEitherDoc <$> ea <*> ea'

-- I'd just use `<>`, but I want to combine errors with a newline in between.
combineEitherDoc :: Semigroup a => Either Doc a -> Either Doc a -> Either Doc a
combineEitherDoc (Left e) (Left e') = Left $ e PP.<$> e'
combineEitherDoc (Left e) _ = Left $ e
combineEitherDoc _ (Left e') = Left $ e'
combineEitherDoc (Right a) (Right a') = Right (a <> a')

instance (Monoid a, Applicative f) => Monoid (ErrT f a) where
        mempty = ErrT . pure . Right $ mempty

mapErr :: Functor f => (Doc -> Doc) -> ErrT f a -> ErrT f a
mapErr f (ErrT fea) = ErrT $ first f <$> fea

raiseErr :: (PP.Pretty e, Applicative f) => e -> ErrT f a
raiseErr = ErrT . pure . Left . PP.pretty

safeFoldr1 :: Foldable t => (a -> a -> a) -> a -> t a -> a
safeFoldr1 _ z ta | null ta = z
safeFoldr1 f _ xs           = foldr1 f xs

sequenceErrs :: Applicative f => [ErrT f a] -> ErrT f [a]
sequenceErrs es = let
        combine = coerce . liftA2 combineEitherDoc `on` runErrT
        singletons = (fmap . fmap) pure es
        in safeFoldr1 combine mempty singletons

orPrintErr :: PP.Pretty e => IO Bool -> e -> ErrT IO ()
orPrintErr q e = ErrT $ flip fmap q $ \case
        True -> Right ()
        False -> Left (PP.pretty e)

instance Monad f => MonadFail (ErrT f) where
        fail = raiseErr
