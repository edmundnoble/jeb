{-# language DerivingVia #-}
{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}

module Jeb.ErrT where

import Control.Monad.Fail
import Control.Monad.Trans.Class(MonadTrans)
import Control.Monad.Trans.Except
import Data.Bifunctor(first)
import Data.Foldable(traverse_)
import Data.Functor(($>))
import Data.Functor.Compose(Compose(..))
import Data.List.NonEmpty(NonEmpty)
import Data.Validation(toEither, fromEither)
import Text.PrettyPrint.ANSI.Leijen(Doc, Pretty(pretty), hardline, putDoc)

newtype ErrT f a = ErrT { runErrT :: f (Either (NonEmpty Doc) a) }
        deriving Functor via ExceptT (NonEmpty Doc) f
        deriving Applicative via ExceptT (NonEmpty Doc) f
        deriving Monad via ExceptT (NonEmpty Doc) f
        deriving MonadTrans via ExceptT (NonEmpty Doc)

instance (Semigroup a, Applicative f) => Semigroup (ErrT f a) where
        (ErrT ea) <> (ErrT ea') = ErrT $ toEither <$>
                ((<>) <$> (fromEither <$> ea) <*> (fromEither <$> ea'))

instance (Monoid a, Applicative f) => Monoid (ErrT f a) where
        mempty = ErrT . pure . Right $ mempty

mapErrs ::
        Functor f =>
        (NonEmpty Doc -> NonEmpty Doc) ->
        ErrT f a -> ErrT f a
mapErrs f (ErrT fea) = ErrT $ first f <$> fea

raiseErrs :: (Pretty e, Applicative f) => NonEmpty e -> ErrT f a
raiseErrs msgs = ErrT . pure . Left $ (pretty <$> msgs)

raiseErr :: (Pretty e, Applicative f) => e -> ErrT f a
raiseErr msg = raiseErrs (pure msg)

sequenceErrs :: (Traversable t, Applicative f) => t (ErrT f a) -> ErrT f (t a)
sequenceErrs es = ErrT $ let
        toComposedValidation = Compose . fmap fromEither . runErrT
        in toEither <$> (getCompose $ traverse toComposedValidation es)

orPrintErr :: IO Bool -> Doc -> ErrT IO ()
orPrintErr q e = ErrT $ flip fmap q $ \case
        True -> Right ()
        False -> Left $ pure e

putDocLn :: Doc -> IO ()
putDocLn d = putDoc (d <> hardline)

terminateErrT :: ErrT IO () -> IO (Maybe ())
terminateErrT (ErrT fea) = fea >>= \case
        Left errs -> traverse_ putDocLn errs $> Nothing
        Right () -> pure $ Just ()

instance Monad f => MonadFail (ErrT f) where
        fail = raiseErr
