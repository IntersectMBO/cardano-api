module Cardano.Api.Tx.UTxO
  ( -- * UTxO type
    UTxO.UTxO (..)

    -- * Operators
  , (UTxO.\\)

    -- * Query
  , UTxO.lookup
  , UTxO.resolveTxIn

    -- * Construction
  , UTxO.empty
  , UTxO.singleton

    -- ** Insertion
  , UTxO.insert

    -- ** Delete/Update
  , UTxO.delete
  , UTxO.adjust

    -- * Combine

    -- ** Union
  , UTxO.union
  , UTxO.unions

    -- ** Difference
  , UTxO.difference

    -- ** Intersection
  , UTxO.intersection

    -- * Traversal

    -- ** Map
  , UTxO.map
  , UTxO.mapWithKey
  , UTxO.mapKeys

    -- ** Fold
  , UTxO.foldMap

    -- * Conversion
  , UTxO.inputSet
  , UTxO.txOutputs

    -- ** Lists
  , UTxO.fromList
  , UTxO.toList

    -- ** Maps
  , UTxO.toMap
  , UTxO.fromMap

    -- ** Shelley
  , UTxO.fromShelleyUTxO
  , UTxO.toShelleyUTxO

    -- * Filter
  , UTxO.filter
  , UTxO.filterWithKey

    -- * Find
  , UTxO.find
  , UTxO.findWithKey
  )
where

import Cardano.Api.Internal.Tx.UTxO qualified as UTxO
