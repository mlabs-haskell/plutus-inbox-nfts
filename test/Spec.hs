module Main (main) where

import Test.Tasty
import Prelude (IO)
import Test.PlutusInboxNfts.Unit qualified as Unit

-- | @since 0.1
main :: IO ()
main = defaultMain tests

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "PlutusInboxNfts"
    [ Unit.tests
    ]
