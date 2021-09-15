import           Test.Tasty

import qualified Cardano.Tracer.Test.Logs.Tests as Logs

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "cardano-tracer"
    [ Logs.tests
    ]
