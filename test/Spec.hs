import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Data.HC3D.ParseSpec (suite)

main :: IO ()
main = defaultMain topSuite

topSuite :: TestTree
topSuite = testGroup "Top-Level Suite"
    [ Data.HC3D.ParseSpec.suite
    ]
