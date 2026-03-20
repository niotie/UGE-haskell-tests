{-# LANGUAGE FlexibleInstances #-}

module Result
  ( Applicative (..),
    MonadFail (..),
    Result,
  )
where

import Prelude
  ( Applicative (..),
    Either (..),
    MonadFail (..),
    String,
  )

type Result = Either String

instance MonadFail Result where
  fail = Left
