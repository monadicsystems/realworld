{-# LANGUAGE QuasiQuotes #-}

module Conduit.Database where

import Conduit.Model
import qualified Hasql.TH as TH -- from "hasql-th"
import Hasql.Statement
import Data.Int

sumStatement :: Statement (Int64, Int64) Int64
sumStatement =
  [TH.singletonStatement|
    select ($1 :: int8 + $2 :: int8) :: int8
    |]

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement =
  [TH.singletonStatement|
    select
      (($1 :: int8) / ($2 :: int8)) :: int8,
      (($1 :: int8) % ($2 :: int8)) :: int8
    |]
