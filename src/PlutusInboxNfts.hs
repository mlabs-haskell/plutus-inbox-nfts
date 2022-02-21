module PlutusInboxNfts (validator) where

import Plutarch.Prelude
import Plutarch.Monadic qualified as P
import Plutarch.Api.V1
  ( PCurrencySymbol
  , PScriptContext
  , PScriptPurpose (PSpending)
  , PTokenName
  , PTxOut
  )
import PlutusInboxNfts.Utils
  ( isSubsetOf
  , passert
  , pfindJust
  , pvalueOf
  )

txOutHasNft :: Term s (PCurrencySymbol :--> PTokenName :--> PAsData PTxOut :--> PBool)
txOutHasNft = phoistAcyclic $ plam $ \nftCS nftTN txOut -> P.do
  let value = pfromData $ pfield @"value" # txOut
  pvalueOf # value # nftCS # nftTN #== 1

validator :: Term s (PCurrencySymbol :--> PTokenName :--> PData :--> PData :--> PScriptContext :--> PUnit)
validator = plam $ \nftCS nftTN _ _ ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  PSpending spentUtxo' <- pmatch $ pfromData ctx.purpose
  txInfo <- pletFields @'["inputs", "outputs"] ctx.txInfo
  inputs <- plet $ pfromData txInfo.inputs
  outputs <- plet $ pfromData txInfo.outputs

  txOutHasNft' <- plet $ txOutHasNft # nftCS # nftTN
  let txInHasNft = plam $ \txInInfo -> txOutHasNft' #$ pfield @"resolved" # txInInfo

  inputValue <- plet $ pfromData $
    pfield @"value" #$
    pfield @"resolved" #$
    pfindJust # txInHasNft # inputs
  outputValue <- plet $ pfromData $
    pfield @"value" #$
    pfindJust # txOutHasNft' # outputs
  spentUtxo <- plet $ pfield @"_0" # spentUtxo'
  spentValue <- plet $ pfromData $
    pfield @"value" #$
    pfield @"resolved" #$
    pfindJust # plam (\input -> pfield @"outRef" # input #== spentUtxo) # inputs

  -- check that outputValue has all necessary asset classes

  passert $ isSubsetOf # inputValue # outputValue
  passert $ isSubsetOf # spentValue # outputValue

  -- check that inputValue + spentValue == outputValue

  let csPairAddsUp = plam $ \csPair -> P.do
        cs <- plet $ pfromData $ pfstBuiltin # csPair
        tnMap <- plet $ pto $ pfromData $ psndBuiltin # csPair
        pall # tnPairAddsUp cs # tnMap

      tnPairAddsUp cs = plam $ \tnPair -> P.do
        tn <- plet $ pfromData $ pfstBuiltin # tnPair
        val <- plet $ pfromData $ psndBuiltin # tnPair
        val #== pvalueOf # inputValue # cs # tn + pvalueOf # spentValue # cs # tn

  passert $ pall # csPairAddsUp #$ pto $ pto outputValue
  pcon PUnit

