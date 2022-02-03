{-# LANGUAGE OverloadedRecordDot #-}
module PlutusInboxNfts (validator) where

import Plutarch.Prelude
import Plutarch.Monadic qualified as P
import Plutarch.Api.V1
  ( PCurrencySymbol
  , PScriptContext
  , PScriptPurpose (PSpending)
  , PTokenName
  , PTxOut
  , PValue
  )

-- | Find the first element that matches the predicate. Partial, throws an error if there's no matching elements.
-- I'm not sure whether it's better than 'phead' after 'pfilter'.
pfindJust :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> a)
pfindJust = phoistAcyclic $
  plam $ \predicate ->
    precList
      ( \self x' xs -> plet x' $ \x ->
          pif
            (predicate # x)
            x
            (self # xs)
      )
      (const perror)

-- | Get the quantity of the given currency in the 'PValue'.
pvalueOf :: Term s (PValue :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $ plam $ \v expectedCS expectedTN -> P.do
  csMap <- plet $ pto $ pto v
  filteredCSMap <- plet $ pfilter # (plam $ \(pfromData . (pfstBuiltin #) -> cs) -> cs #== expectedCS) # csMap
  pmatch filteredCSMap $ \case
    PNil -> 0
    PCons csHead _ -> P.do
      tnMap <- plet $ pfromData $ psndBuiltin # csHead
      filteredTNMap <- plet $ pfilter # (plam $ \(pfromData . (pfstBuiltin #) -> tn) -> tn #== expectedTN) # pto tnMap
      pmatch filteredTNMap $ \case
        PNil -> 0
        PCons tnHead _ -> pfromData $ psndBuiltin # tnHead

txOutHasNft :: Term s (PCurrencySymbol :--> PTokenName :--> PAsData PTxOut :--> PBool)
txOutHasNft = phoistAcyclic $ plam $ \nftCS nftTN txOut -> P.do
  let value = pfromData $ pfield @"value" # txOut
  pvalueOf # value # nftCS # nftTN #== 1

passert :: forall (s :: S) (a :: PType). Term s PBool -> Term s a -> Term s a
passert b inp = pif b inp perror

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
  outputValue <- plet $ pto $ pto $ pfromData $
    pfield @"value" #$
    pfindJust # txOutHasNft' # outputs
  spentUtxo <- plet $ pfield @"_0" # spentUtxo'
  spentValue <- plet $ pfromData $
    pfield @"value" #$
    pfield @"resolved" #$
    pfindJust # plam (\input -> pfield @"outRef" # input #== spentUtxo) # inputs

  -- check that inputValue + spentValue == outputValue

  let csPairAddsUp = plam $ \csPair -> P.do
        cs <- plet $ pfromData $ pfstBuiltin # csPair
        tnMap <- plet $ pto $ pfromData $ psndBuiltin # csPair
        pall # tnPairAddsUp cs # tnMap

      tnPairAddsUp cs = plam $ \tnPair -> P.do
        tn <- plet $ pfromData $ pfstBuiltin # tnPair
        val <- plet $ pfromData $ psndBuiltin # tnPair
        val #== pvalueOf # inputValue # cs # tn + pvalueOf # spentValue # cs # tn

  passert $ pall # csPairAddsUp # outputValue
  pcon PUnit

