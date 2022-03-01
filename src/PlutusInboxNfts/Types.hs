{-# LANGUAGE UndecidableInstances #-}

module PlutusInboxNfts.Types
  ( PAssetClass (PAssetClass)
  ) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic)

import Plutarch.Prelude
import Plutarch.Api.V1 (PCurrencySymbol, PTokenName)
import Plutarch.DataRepr

newtype PAssetClass (s :: S) =
  PAssetClass
    ( Term
        s
        ( PDataRecord
            '[ "currencySymbol" ':= PCurrencySymbol
             , "tokenName" ':= PTokenName
             ]
        )
    )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving (PMatch, PIsData, PDataFields) via PIsDataReprInstances PAssetClass
--via (DerivePNewtype PAssetClass (PBuiltinPair PCurrencySymbol PTokenName))

