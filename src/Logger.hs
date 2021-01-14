{-# LANGUAGE InstanceSigs #-}

module Logger where

import Prelude hiding (log)

newtype Logger a = Logger {runLogger :: ([String], a)}

instance Functor Logger where
  fmap :: (a -> b) -> Logger a -> Logger b
  fmap f (Logger (acc, a)) = Logger (acc, f a)

instance Applicative Logger where
  pure :: a -> Logger a
  pure a = Logger ([], a)
  (<*>) :: Logger (a -> b) -> Logger a -> Logger b
  (<*>) (Logger (accX, f)) (Logger (accY, a)) =
    Logger (accX <> accY, f a)

instance Monad Logger where
  return = pure
  (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  (>>=) (Logger (accX, a)) f = Logger (accX <> accY, b)
    where
      (Logger (accY, b)) = f a

log :: String -> Logger ()
log s = Logger ([s], ())

logs :: [String] -> Logger ()
logs ss = Logger (ss, ())

-- EXERCISES ---

addTwo :: Int -> Logger Int
addTwo x = x + 2 <$ log "adding 2..."

augmentAndStringify :: Int -> Int -> Logger String
augmentAndStringify x y = do
  log "augmenting..."
  x' <- addTwo x
  y' <- addTwo y
  log "stringifying..."
  pure $ show (x' + y')
