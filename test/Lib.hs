module Lib where

import Test.Hspec ( HasCallStack, shouldSatisfy, Expectation )

--ALL of cases SHOULD SATISFY to one predicate
allShouldSatisfy :: (HasCallStack, Show a) => [a] -> (a -> Bool) -> Expectation
allShouldSatisfy cases f = biMapM_ shouldSatisfy cases (replicate (length cases) f)

-- This is not equal to base Data.Bifoldable.bimapM_
biMapM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
biMapM_ _ [] [] = return ()
biMapM_ f (x : xs) (y : ys) = do
  _ <- f x y
  biMapM_ f xs ys
biMapM_ _ _ _ = return ()