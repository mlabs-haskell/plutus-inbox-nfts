{-# LANGUAGE UndecidableInstances #-}

module PlutusInboxNfts.Types
  ( PAssetClass (PAssetClass)
  , mkPAssetClass
  ) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Plutarch.Prelude
import Plutarch.Api.V1 (PCurrencySymbol, PTokenName)
import Plutarch.DataRepr (PDataFields, PIsDataReprInstances (PIsDataReprInstances))

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
  deriving anyclass (Generic, PIsDataRepr)
  deriving (PlutusType, PIsData, PDataFields) via PIsDataReprInstances PAssetClass

mkPAssetClass :: Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
mkPAssetClass = phoistAcyclic $ plam $ \cs tn -> pcon $
  PAssetClass $
    pdcons @"currencySymbol" # pdata cs #$
    pdcons @"tokenName" # pdata tn #$
    pdnil

