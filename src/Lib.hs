{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( someFunc,
    reverseWithCount,
    appendReversedWithCount,
    append3ReversedWithCount,
    State (..),
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  --      (a -> b) -> (s -> (s, a)) -> (s -> (s, b))
  fmap f (State stateFn) =
    State
      ( \s ->
          let (s', a) = stateFn s
           in (s', f a)
      )

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (,a)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  --       (s -> (a -> b))  -> (s -> (s, a)) -> (s -> (s, b))
  (<*>) (State stateF) (State stateA) =
    State
      ( \s ->
          let (s', f) = stateF s
              (s'', a) = stateA s'
           in (s'', f a)
      )

instance Monad (State s) where
  return :: a -> State s a
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State stateA) f =
    State
      ( \s ->
          let (s', a) = stateA s
              (State stateB) = f a
           in stateB s'
      )

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State $ const (s, ())

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

reverseWithCount :: [a] -> State Int [a]
reverseWithCount list =
  reverse list <$ modify (+ 1)

appendReversedWithCount :: [a] -> [a] -> State Int [a]
appendReversedWithCount as1 as2 = do
  as1Rev <- reverseWithCount as1
  as2Rev <- reverseWithCount as2
  (as1Rev ++ as2Rev) <$ modify (+ 1)

append3ReversedWithCount :: [a] -> [a] -> [a] -> State Int [a]
append3ReversedWithCount as1 as2 as3 = do
  as1Rev <- reverseWithCount as1
  as2Rev <- reverseWithCount as2
  as3Rev <- reverseWithCount as3
  (as1Rev ++ as2Rev ++ as3Rev) <$ modify (+ 1)
