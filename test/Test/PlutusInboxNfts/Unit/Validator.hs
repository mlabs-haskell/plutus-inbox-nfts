module Test.PlutusInboxNfts.Unit.Validator (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval qualified as Interval

import Plutarch.Api.V1
import Plutarch.Prelude

import PlutusInboxNfts (validator)

import Utils

buildCtx
  :: Value -- locked value spent by tx
  -> [Value] -- other inputs
  -> [Value] -- outputs
  -> Term s PScriptContext
buildCtx spentValue otherInputValues outputValues = pconstant $ ScriptContext info purpose
  where
    purpose :: ScriptPurpose
    purpose = Spending spentRef

    info :: TxInfo
    info =
      TxInfo
        { txInfoInputs = spentUtxo : inputUtxos
        , txInfoOutputs = outputUtxos
        , txInfoFee = mempty
        , txInfoMint = mempty
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = Interval.always
        , txInfoSignatories = []
        , txInfoData = []
        , txInfoId = "b0"
        }

    spentUtxo :: TxInInfo
    spentUtxo =
      TxInInfo
        { txInInfoOutRef = spentRef
        , txInInfoResolved =
            TxOut
              { txOutAddress =
                  Address (ScriptCredential validatorHash) Nothing
              , txOutValue = spentValue
              , txOutDatumHash = Nothing
              }
        }

    inputUtxos :: [TxInInfo]
    inputUtxos = flip map otherInputValues $ \inputValue ->
      TxInInfo
        { txInInfoOutRef = spentRef -- just a mock value
        , txInInfoResolved =
            TxOut
              { txOutAddress =
                  Address (ScriptCredential validatorHash) Nothing
              , txOutValue = inputValue
              , txOutDatumHash = Nothing
              }
        }

    outputUtxos :: [TxOut]
    outputUtxos = flip map outputValues $ \outputValue ->
      TxOut
        { txOutAddress =
            Address (ScriptCredential validatorHash) Nothing
        , txOutValue = outputValue
        , txOutDatumHash = Nothing
        }

    validatorHash :: ValidatorHash
    validatorHash = "a1"

    spentRef :: TxOutRef
    spentRef = TxOutRef "a0" 0

nftCoin :: Value
nftCoin = singleton nftCS nftTN 1

-- | Construct a value of n coins of some currency identified by token name; just for brevity
coins :: TokenName -> Integer -> Value
coins = singleton "c3"

nftCS :: CurrencySymbol
nftCS = "c1"

nftTN :: TokenName
nftTN = "NFT"

runValidatorWithCtx :: Term s (PScriptContext :--> PUnit)
runValidatorWithCtx = phoistAcyclic $
  plam $ \ctx ->
    let unit = pconstant (I 0)
    in validator # pconstant nftCS # pconstant nftTN # unit # unit # ctx

tests :: HasTester => TestTree
tests = do
  testGroup
    "validator tests"
    [ testCase "success path" $ do
        let lockedValue = coins "A" 10
            inputValue = coins "A" 20 <> nftCoin
        succeeds $ runValidatorWithCtx # buildCtx lockedValue [inputValue] [lockedValue <> inputValue]
    , testCase "fails; no NFT" $ do
        fails $ runValidatorWithCtx # buildCtx (coins "A" 10) [coins "A" 20] [coins "A" 30]
    , testCase "fails; amount of some asset class differs" $ do
        fails $ runValidatorWithCtx # buildCtx (coins "A" 10) [nftCoin] [nftCoin <> coins "A" 5, coins "A" 5]
    , testCase "fails; lacks some amount of an asset class at output" $ do
        fails $ runValidatorWithCtx # buildCtx (coins "A" 10) [nftCoin] [nftCoin]
    , testCase "fails; redundant asset class at output" $ do
        fails $ runValidatorWithCtx # buildCtx (coins "A" 10) [nftCoin] [nftCoin <> coins "A" 10 <> coins "B" 10]
    ]

