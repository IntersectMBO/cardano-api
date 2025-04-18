{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module Cardano.Api.Internal.Orphans.All
  ( AsType
        ( AsColdCommitteeCredential
        , AsHotCommitteeCredential
        , AsDrepCredential
        , AsGovActionId
        )
  )
where

import Cardano.Api.Internal.Orphans.Misc ()
import Cardano.Api.Internal.Orphans.Serialization (AsType
        ( AsColdCommitteeCredential
        , AsHotCommitteeCredential
        , AsDrepCredential
        , AsGovActionId
        ))
import Cardano.Api.Internal.Orphans.Serialization ()