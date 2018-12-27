{-# language DerivingVia #-}
{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}

module Jeb.ErrT where

import Control.Monad.Fail
import Control.Monad.Trans.Class(MonadTrans)
import Control.Monad.Trans.Except
import Data.Foldable(traverse_)
import Data.Functor(($>))
import Data.Functor.Compose(Compose(..))
import Data.Validation(toEither, fromEither)

newtype ErrT f a = ErrT { runErrT :: f (Either (IO ()) a) }
        deriving Functor via ExceptT (IO ()) f
        deriving Applicative via ExceptT (IO ()) f
        deriving Monad via ExceptT (IO ()) f
        deriving MonadTrans via ExceptT (IO ())

instance (Semigroup a, Applicative f) => Semigroup (ErrT f a) where
        (ErrT ea) <> (ErrT ea') = ErrT $ toEither <$>
                ((<>) <$> (fromEither <$> ea) <*> (fromEither <$> ea'))

instance (Monoid a, Applicative f) => Monoid (ErrT f a) where
        mempty = ErrT . pure . Right $ mempty

errs :: Applicative f => [String] -> ErrT f a
errs msgs = ErrT . pure . Left $ traverse_ putStrLn msgs

err :: Applicative f => String -> ErrT f a
err msg = errs [msg]

sequenceErrs :: (Traversable t, Applicative f) => t (ErrT f a) -> ErrT f (t a)
sequenceErrs es = ErrT $ let
        toComposedValidation = Compose . fmap fromEither . runErrT
        in toEither <$> (getCompose $ traverse toComposedValidation es)

orPrintErr :: IO Bool -> String -> ErrT IO ()
orPrintErr q e = ErrT $ flip fmap q $ \case
        True -> Right ()
        False -> Left (putStrLn e)

terminateErrT :: ErrT IO () -> IO (Maybe ())
terminateErrT (ErrT fea) = fea >>= \case
        Left printErrs -> printErrs $> Nothing
        Right () -> pure $ Just ()

instance Monad f => MonadFail (ErrT f) where
        fail = err
