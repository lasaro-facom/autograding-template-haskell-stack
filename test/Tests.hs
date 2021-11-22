import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Exercise (hello)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} specs

specs :: Spec
specs = do
        it "Returns a Hello, Haskell! string " $
          hello `shouldBe` "Hello, Haskell!"
        it "Returns a Hello, Haskell! string, again " $
          hello `shouldBe` "Hello, Haskell!"
