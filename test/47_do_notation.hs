import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

fooMonad :: Maybe String
fooMonad = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

main :: IO()
main = hspec $ do
    describe "The do notation" $ do
        it "can combine functions with Maybe values" $ do
            (Just 3 >>= (\x -> Just (show x ++ "!")))
                `shouldBe` Just "3!"
            (Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))))
                `shouldBe` Just "3!"
            (Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing)))
                `shouldBe` (Nothing :: Maybe String)
        it "can be wrapped on different lines" $ do
            foo `shouldBe` Just "3!"
            fooMonad `shouldBe` Just "3!"

