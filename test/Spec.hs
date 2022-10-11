import Test.Hspec
import DhallToPythonLib
import Dhall.Parser
import Dhall.Core

main :: IO ()
main = hspec $ do
    describe "Parse basic dhall expressions" $ do
        it "parses a natural number to a dhall Natural" $ do
            let expected = NaturalLit 21 :: Expr Src Import
            let actual = exprFromText "Simple Natural" "21" :: Either ParseError (Expr Src Import)
            case actual of
                Left err -> 1 `shouldBe` 0
                Right expr -> case expr of
                    Note _ val -> val `shouldBe` expected
                    _ -> 1 `shouldBe` 0

