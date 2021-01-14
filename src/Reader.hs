{-# LANGUAGE InstanceSigs #-}

module Reader where

import Data.Char as Char

-- Our config data. We're very particular about certain
-- letters of the alphabet, see.
data ABConfig = ABConfig
  { don'tUseLetterE :: Bool,
    don'tUseLetterL :: Bool
  }

newtype Reader cfg a = Reader {runReader :: cfg -> a}

instance Functor (Reader cfg) where
  fmap :: (a -> b) -> Reader cfg a -> Reader cfg b
  --      (a -> b) -> (cfg -> a)   -> (cfg -> b)
  fmap f (Reader cfgA) = Reader (f . cfgA)

instance Applicative (Reader cfg) where
  pure :: a -> Reader cfg a
  pure = Reader . const
  (<*>) :: Reader cfg (a -> b) -> Reader cfg a -> Reader cfg b
  --       (cfg -> a -> b)     -> (cfg -> a)   -> (cfg -> b)
  (<*>) (Reader cfgF) (Reader cfgA) =
    Reader (\cfg -> cfgF cfg (cfgA cfg))

instance Monad (Reader cfg) where
  return :: a -> Reader cfg a
  return = pure
  (>>=) :: Reader cfg a -> (a -> Reader cfg b) -> Reader cfg b
  --       (cfg -> a)   -> (a -> cfg -> b)     -> (cfg -> b)
  (>>=) (Reader cfgA) f =
    Reader (\cfg -> (runReader $ f $ cfgA cfg) cfg)

ask :: Reader cfg cfg
ask = Reader id

asks :: (cfg -> a) -> Reader cfg a
asks = Reader

local :: (cfg -> cfg') -> Reader cfg' a -> Reader cfg a
local transform (Reader f) =
  Reader $ f . transform

-- EXERCISES ---

-- Uppercase the string, obeying current tests.
toUpperStr :: String -> Reader ABConfig String
toUpperStr str = do
  forE <- asks don'tUseLetterE
  forL <- asks don'tUseLetterL
  let filters :: [Char -> Bool]
      filters =
        [ if forE then (/= 'E') else const True,
          if forL then (/= 'L') else const True
        ]

      passesFilters :: Char -> Bool
      passesFilters c = all (\f -> f c) filters
  pure $ filter passesFilters (fmap Char.toUpper str)

fullName :: String -> String -> String -> Reader ABConfig String
fullName firstName lastName nickName = do
  a <- toUpperStr firstName
  b <- toUpperStr lastName
  c <- toUpperStr nickName
  pure $ a <> " '" <> c <> "' " <> b
