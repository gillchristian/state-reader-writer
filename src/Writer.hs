{-# LANGUAGE InstanceSigs #-}

module Writer where

import Prelude hiding (log)

newtype Writer log a = Writer {runWriter :: (log, a)}

instance Functor (Writer log) where
  fmap :: (a -> b) -> Writer log a -> Writer log b
  fmap f (Writer (log, a)) = Writer (log, f a)

instance Monoid log => Applicative (Writer log) where
  pure :: a -> Writer log a
  pure a = Writer (mempty, a)
  (<*>) :: Writer log (a -> b) -> Writer log a -> Writer log b
  (<*>) (Writer (logX, f)) (Writer (logY, a)) =
    Writer (logX <> logY, f a)

instance Monoid log => Monad (Writer log) where
  return = pure
  (>>=) :: Writer log a -> (a -> Writer log b) -> Writer log b
  (>>=) (Writer (logX, a)) f = Writer (logX <> logY, b)
    where
      (Writer (logY, b)) = f a

tell :: log -> Writer log ()
tell log = Writer (log, ())

censor :: (log -> log) -> Writer log a -> Writer log a
censor f (Writer (log, a)) = Writer (f log, a)

listen :: Writer log a -> Writer log (a, log)
listen (Writer (log, a)) = Writer (log, (a, log))

-- EXERCISES ---

log :: String -> Writer [String] ()
log msg = tell [msg]

addTwo :: Int -> Writer [String] Int
addTwo x = x + 2 <$ log "adding 2..."

augmentAndStringify :: Int -> Int -> Writer [String] String
augmentAndStringify x y = do
  log "augmenting..."
  x' <- addTwo x
  y' <- addTwo y
  log "stringifying..."
  pure $ show (x' + y')
