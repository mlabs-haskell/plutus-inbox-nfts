module PlutusInboxNfts.Utils (pfindJust, pvalueOf, passert) where

import Plutarch.Prelude
import Plutarch.Monadic qualified as P
import Plutarch.Api.V1
  ( PCurrencySymbol
  , PTokenName
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

passert :: forall (s :: S) (a :: PType). Term s PBool -> Term s a -> Term s a
passert b inp = pif b inp perror

