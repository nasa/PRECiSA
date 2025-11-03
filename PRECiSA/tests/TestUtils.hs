{-# LANGUAGE ScopedTypeVariables #-}
module TestUtils where

import Test.Tasty.HUnit (assertFailure, Assertion)
import Control.Exception (evaluate, try, SomeException)

throwsException :: forall a. a -> Assertion
throwsException action = do
      result <- try (evaluate action) :: IO (Either SomeException a)
      case result of
        Left _  -> return ()
        Right _ -> assertFailure "Expected an exception, but got a value"
