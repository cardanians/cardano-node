
import           Cardano.Prelude
import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Node.FilePermissions
import qualified Test.Cardano.Node.Json
import qualified Test.Cardano.Node.POM

main :: IO ()
main = defaultMain
    [ Test.Cardano.Node.FilePermissions.tests
    , Test.Cardano.Node.Json.tests
    , Test.Cardano.Node.POM.tests
    ]
