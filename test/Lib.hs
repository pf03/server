module Lib where

import Control.Exception (evaluate)
import Control.Monad.State.Lazy (State, runState)
import Data.Either ()
import Interface.Error as Error ()
import Test.Hspec
  ( Expectation,
    HasCallStack,
    shouldBe,
    shouldSatisfy,
  )

-----------------------------Mutiple cases-------------------------------------
--ALL of cases SHOULD BE eqaul to one result
allShouldBe :: (HasCallStack, Show a, Eq a) => [a] -> a -> Expectation
allShouldBe cases result = eachShouldBe cases (replicate (length cases) result)

--EACH of cases SHOULD BE eqaul to each of results
eachShouldBe :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
eachShouldBe = bimapM_ shouldBe

--ALL of cases SHOULD SATISFY to one predicat
allShouldSatisfy :: (HasCallStack, Show a) => [a] -> (a -> Bool) -> Expectation
allShouldSatisfy cases f = bimapM_ shouldSatisfy cases (replicate (length cases) f)

-----------------------------State cases---------------------------------------
-- Can't use bimapM_ in this functions, because they do not consider State effect
-- ALL EVAL STATES of cases SHOULD BE eqaul to one result WITH INITIAL STATE
allEvalStatesShouldBe :: (HasCallStack, Show a, Eq a) => [State s a] -> (a, s) -> Expectation
allEvalStatesShouldBe states (result, initialState) = eachEvalStateShouldBe states (replicate (length states) result, initialState)

--EACH EVAL STATE of cases SHOULD BE equal to each of results WITH INITIAL STATE
eachEvalStateShouldBe :: (HasCallStack, Show a, Eq a) => [State s a] -> ([a], s) -> Expectation
eachEvalStateShouldBe [] ([], _) = return ()
eachEvalStateShouldBe (s : ss) (r : rs, initialState) = do
  let (a, modifiedState) = runState s initialState
  a `shouldBe` r
  eachEvalStateShouldBe ss (rs, modifiedState)
eachEvalStateShouldBe _ _ = fail "lists of tests and answers must have equal lengths"

--EVAL STATE of case SHOULD BE equal to result WITH INITIAL STATE
evalStateShouldBe :: (HasCallStack, Show a, Eq a) => State s a -> (a, s) -> Expectation
evalStateShouldBe state (result, initialState) = eachEvalStateShouldBe [state] ([result], initialState)

withInitialState :: a -> b -> (a, b)
withInitialState = (,)

-- This is not equal to base Data.Bifoldable.bimapM_
bimapM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
bimapM_ f [] [] = return ()
bimapM_ f (x : xs) (y : ys) = do
  f x y
  bimapM_ f xs ys
bimapM_ _ _ _ = return ()