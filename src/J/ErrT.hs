{-# language DerivingVia #-}
{-# language LambdaCase #-}

module J.ErrT where

import Control.Monad.Fail
import Control.Monad.Trans.Class(MonadTrans)
import Control.Monad.Trans.Except
import Data.Foldable(traverse_)
import Data.Functor(($>))

newtype ErrT f a = ErrT (f (Either (IO ()) a))
        deriving Functor via ExceptT (IO ()) f
        deriving Applicative via ExceptT (IO ()) f
        deriving Monad via ExceptT (IO ()) f
        deriving MonadTrans via ExceptT (IO ())

errs :: Applicative f => [String] -> ErrT f a
errs msgs = ErrT (pure $ Left $ traverse_ putStrLn msgs)

err :: Applicative f => String -> ErrT f a
err msg = errs [msg]

sequenceErrs :: Monad f => [ErrT f a] -> ErrT f [a]
sequenceErrs [] = pure []

runErrT :: ErrT IO () -> IO (Maybe ())
runErrT (ErrT fea) = fea >>= \case
        Left printErrs -> printErrs $> Nothing
        Right () -> pure $ Just ()

instance Monad f => MonadFail (ErrT f) where
        fail = err
