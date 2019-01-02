{-# language DerivingVia #-}
{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}

module Jeb.ErrT where

import Control.Monad.Fail
import Control.Monad.Trans.Class(MonadTrans)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class(MonadIO)
import Data.Bifunctor(first)
import Data.Functor(($>))
import Data.Functor.Compose(Compose(..))
import Data.Validation(toEither, fromEither)
import Text.PrettyPrint.ANSI.Leijen((<+>), Doc, Pretty(pretty), hardline, putDoc)

newtype ErrT f a = ErrT { runErrT :: f (Either Doc a) }
        deriving Functor via ExceptT Doc f
        deriving Applicative via ExceptT Doc f
        deriving Monad via ExceptT Doc f
        deriving MonadTrans via ExceptT Doc
        deriving MonadIO via ExceptT Doc f

instance (Semigroup a, Applicative f) => Semigroup (ErrT f a) where
        (ErrT ea) <> (ErrT ea') = ErrT $ -- toEither <$>
                combineEitherDoc <$> ea <*> ea'

-- I'd just use `<>`, but I want to combine errors with a newline in between.
combineEitherDoc :: Semigroup a => Either Doc a -> Either Doc a -> Either Doc a
combineEitherDoc (Left e) (Left e') = Left $ e <+> e'
combineEitherDoc (Left e) _ = Left $ e
combineEitherDoc _ (Left e') = Left $ e'
combineEitherDoc (Right a) (Right a') = Right (a <> a')

instance (Monoid a, Applicative f) => Monoid (ErrT f a) where
        mempty = ErrT . pure . Right $ mempty

mapErr :: Functor f => (Doc -> Doc) -> ErrT f a -> ErrT f a
mapErr f (ErrT fea) = ErrT $ first f <$> fea

raiseErr :: (Pretty e, Applicative f) => e -> ErrT f a
raiseErr = ErrT . pure . Left . pretty

sequenceErrs :: (Traversable t, Applicative f) => t (ErrT f a) -> ErrT f (t a)
sequenceErrs es = ErrT $ let
        toComposedValidation = Compose . fmap fromEither . runErrT
        in toEither <$> (getCompose $ traverse toComposedValidation es)

orPrintErr :: Pretty e => IO Bool -> e -> ErrT IO ()
orPrintErr q e = ErrT $ flip fmap q $ \case
        True -> Right ()
        False -> Left (pretty e)

putDocLn :: Doc -> IO ()
putDocLn = putDoc . (<> hardline)

terminateErrT :: ErrT IO () -> IO (Maybe ())
terminateErrT (ErrT fea) = fea >>= \case
        Left e -> putDocLn e $> Nothing
        Right () -> pure $ Just ()

instance Monad f => MonadFail (ErrT f) where
        fail = raiseErr
