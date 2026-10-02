{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Rpc.Pagination where

import Cardano.Api (TxIn)
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Tx (parseTxIn, renderTxIn)
import Cardano.Rpc.Server.Internal.UtxoRpc.Query (paginateByTxIn)

import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IsList (fromList)
import GHC.Stack (HasCallStack)

import Hedgehog as H
import Hedgehog.Extras qualified as H

-- ---------------------------------------------------------------------------
-- Pagination
-- ---------------------------------------------------------------------------

hprop_paginate_empty_input :: Property
hprop_paginate_empty_input = H.propertyOnce $ do
  let (page, nextToken) = paginateByTxIn (const True) (fromList [] :: Map TxIn ()) Nothing 10
  page === []
  H.assertWith nextToken isNothing

hprop_paginate_all_items_fit :: Property
hprop_paginate_all_items_fit = H.propertyOnce $ do
  let items = fromList [mkItem 0, mkItem 1, mkItem 2]
      (page, nextToken) = paginateByTxIn (const True) items Nothing 10
  length page === 3
  H.assertWith nextToken isNothing

hprop_paginate_respects_limit :: Property
hprop_paginate_respects_limit = H.propertyOnce $ do
  let items = fromList [mkItem i | i <- [0 .. 9]]
      (page, nextToken) = paginateByTxIn (const True) items Nothing 3
  length page === 3
  H.assertWith nextToken isJust

hprop_paginate_multi_digit_indices :: Property
hprop_paginate_multi_digit_indices = H.propertyOnce $ do
  -- Regression: indices 5 and 10 must not produce duplicates across pages
  let items = fromList [mkItem i | i <- [0, 1, 2, 5, 10, 11, 20]]
      (page1, token1) = paginateByTxIn (const True) items Nothing 3
  length page1 === 3
  H.assertWith token1 isJust
  let (page2, token2) = paginateByTxIn (const True) items token1 3
  length page2 === 3
  H.assertWith token2 isJust
  let (page3, token3) = paginateByTxIn (const True) items token2 3
  length page3 === 1
  H.assertWith token3 isNothing
  -- All items returned exactly once
  let allRendered = map (renderTxIn . fst) $ page1 ++ page2 ++ page3
  length allRendered === 7

hprop_paginate_default_limit :: Property
hprop_paginate_default_limit = H.propertyOnce $ do
  let items = fromList [mkItem i | i <- [0 .. 49]]
      (page, nextToken) = paginateByTxIn (const True) items Nothing 0
  -- Default page size is 100; all 50 items fit
  length page === 50
  H.assertWith nextToken isNothing

hprop_paginate_sequential_pages_cover_all :: Property
hprop_paginate_sequential_pages_cover_all = H.propertyOnce $ do
  let items = fromList [mkItem i | i <- [0 .. 4]]
      (page1, token1) = paginateByTxIn (const True) items Nothing 2
  length page1 === 2
  let (page2, token2) = paginateByTxIn (const True) items token1 2
  length page2 === 2
  let (page3, token3) = paginateByTxIn (const True) items token2 2
  length page3 === 1
  H.assertWith token3 isNothing

hprop_paginate_cursor_excludes_start_token :: Property
hprop_paginate_cursor_excludes_start_token = H.propertyOnce $ do
  -- The start token names the last TxIn already seen; the next page must
  -- resume strictly after it, never repeating it.
  let items = fromList [mkItem i | i <- [0 .. 9]]
      cursor = fst $ mkItem 4
      (page, _) = paginateByTxIn (const True) items (Just $ renderTxIn cursor) 3
  map fst page === map (fst . mkItem) [5, 6, 7]
  H.assertWith page (notElem cursor . map fst)

hprop_paginate_predicate_limits_and_advances_by_matches :: Property
hprop_paginate_predicate_limits_and_advances_by_matches = H.propertyOnce $ do
  -- Ten items, five of which (0, 2, 4, 6, 8) match; the limit and the next
  -- token must both be counted over matches only, not over all items.
  let items = fromList [mkItem i | i <- [0 .. 9]]
      (page5, token5) = paginateByTxIn even items Nothing 5
  length page5 === 5
  H.assertWith page5 (all $ even . snd)
  H.assertWith token5 isNothing
  let (page3, token3) = paginateByTxIn even items Nothing 3
  map snd page3 === [0, 2, 4]
  token3 === Just (renderTxIn . fst $ mkItem 4)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

hash01 :: Text
hash01 = "0101010101010101010101010101010101010101010101010101010101010101"

mkTxIn :: HasCallStack => Text -> TxIn
mkTxIn = either error id . P.runParser parseTxIn

mkItem :: Int -> (TxIn, Int)
mkItem i = (mkTxIn $ hash01 <> "#" <> T.show i, i)
