{-
   Monads are just beefed up applicative functors, much like
   applicative functors are only beefed up functor.s

   (>>=) :: (Monad m) => m a -> (a -> m b) -> m b 
   pronounced - bind
-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- Let's not use >>=, create an applyMaybe fn
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

main :: IO()
main = hspec $ do
    describe "Monads" $ do
        it "can wrap a normal value into a Just value" $ do
            (\x -> Just (x+1)) 1 `shouldBe` Just 2
            (\x -> Just (x+1)) 100 `shouldBe` Just 101
        it "can apply function to Maybe values" $ do
            (Just 3 `applyMaybe` \x -> Just (x+1))
                `shouldBe` Just 4
            (Just "smile" `applyMaybe` \x -> Just (x ++ " :)"))
                `shouldBe` Just "smile :)"
            (Nothing `applyMaybe` \x -> Just (x+1))
                `shouldBe` Nothing
            (Nothing `applyMaybe` \x -> Just (x ++ " :)"))
                `shouldBe` Nothing
        it "checks if value is >2" $ do
            (Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing)
                `shouldBe` Just 3
            (Nothing `applyMaybe` \x -> if x > 2 then Just x else Nothing)
                `shouldBe` Nothing
        it "works like our applyMaybe fn" $ do
            let value = return "WHAT" :: Maybe String
            value `shouldBe` Just "WHAT"
            (Just 9 >>= \x -> return (x*10)) `shouldBe` Just 90
            (Nothing >>= \x -> return (x*10)) `shouldBe` Nothing
