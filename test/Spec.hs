{-
You are not required to edit this file, but do feel free to take a look. The 
code in here is implemented in a deliberately poor way, without comments or 
guidance. In my opinion, decoding the approaches taken here is *much* more 
difficult than implementing the work properly yourself :)
-}
{-
Tasty is the testing library that is used to specify tests.
The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit 
tests and property tests (respectively) are written.
-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-all #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import Ants.Types
import Ants

import System.Console.ANSI (clearScreen)
import Test.Tasty
  ( TestTree, testGroup,
  )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (findIndex, nub, inits, isInfixOf)
import Data.Function (on)
import Data.Tuple (swap)

import Data.Set (Set)
import qualified Data.Set as Set
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (pack)

main :: IO ()
main = do
  clearScreen
  muffledMain $
    testGroup
      "Tests"
      [ testShowLineState
      , testRuleX
      , testApplyRule
      , testLoopedAt
      , testAllRules
      , testInitialState
      , testLeftRight
      , testStep
      ]

testShowLineState :: TestTree
testShowLineState = testGroup "Ex. 1: Show LineState"
  [ testCase "Renders the empty string properly"
      $ show (LS []) @?= "||"
  , testProperty "Renders a blank line of length n properly"
      $ forAllShrink ((\n -> replicate n Off) <$> chooseInt (0,100)) subterms  $ \n ->
        show (LS n) == "|" ++ replicate (length n) ' ' ++ "|"
  , testProperty "Renders arbitrary lines properly"
      $ let
        f Off ' ' = True
        f On  '█' = True
        f _   _   = False
      in forAll (listOf $ elements [Off,On]) $ \cs ->
        and (zipWith f cs (tail (init (show (LS cs)))))
  ]

testRuleX :: TestTree
testRuleX = testGroup "Ex. 2: ruleX"
  [ testCase "ruleX behaves correctly"
      $ let
          sq = (,,) <$> [Off,On] <*> [Off,On] <*> [Off,On]
          res = (\f (a,b,c) -> f a b c) ruleX <$> sq
          e = \case { Off -> 0; _ -> 1 }
          k = foldr (\x n -> n*2 + e x) 0 res
        in
          assertBool "ruleX is incorrect in some way." (k == 90)
  ]

testApplyRule :: TestTree
testApplyRule = testGroup "Ex. 3: applyRule"
  [ testProperty "Never changes the number of cells"
      $ forAll (listOf $ elements [Off,On]) $ \cs ->
        length ((\(LS s) -> s) $ applyRule ruleX (LS cs)) == length cs
  , testGroup "Behaves correctly on..."
    [ testCase "The empty state" $ applyRule ruleX (LS [])
        @?= LS []
    , testCase "Example 1" $ applyRule ruleX (LS [On,On])
        @?= LS [On,On]
    , testCase "Example 2" $ applyRule ruleX (LS [Off,On,Off])
        @?= LS [On,Off,On]
    , testCase "Example 3" $ applyRule rule0 (LS [Off,On,On,Off])
        @?= LS [Off,Off,Off,Off]
    , testCase "Example 4" $ let
      ruleLeft = (\l _ _ -> if l == On then On else Off)
      in iterate (applyRule ruleLeft) (LS (On : replicate 15 Off)) !! 15
        @?= LS (replicate 15 Off ++ [On])
    ]
  ]

testLoopedAt :: TestTree
testLoopedAt = testGroup "Ex. 4: loopedAt"
  [ testCase "Behaves correctly for Example 1"
      $ loopedAt rule0 (LS [Off, On, Off, On, Off, On]) @=? 2
  ]

rule0 :: CellState -> CellState -> CellState -> CellState
rule0 a b c = Off

testAllRules :: TestTree
testAllRules = testGroup "Ex. 5: allRules"
  [ testCase "allRules has 256 elements"
      $ length allRules @?= 256
  , testCase "...and they all work differently"
      $ let fullState = LS [Off,Off,Off,Off,Off,On,Off,On,Off,Off,On,On,On,Off,Off,On,Off,On,On,On,Off,On,On,On]
      in length (nub $ map (`applyRule` fullState) allRules) @?= 256
  ]

testInitialState :: TestTree
testInitialState = testGroup "Ex. 6: initialState"
  [ testCase "Starts at the origin"
      $ assertBool "The ant is not set to start at the origin."
      $ let AS _ p _ = initialState in
        uncurry (^) p == 1 && uncurry (^) (swap p) == 1
  , testCase "Starts facing West"
      $ assertBool "The ant does not start facing west."
      $ 'W' `elem` show initialState
  ]

testLeftRight :: TestTree
testLeftRight = testGroup "Ex. 7: leftOf and rightOf"
  [ testCase "left behaves correctly"
      $ assertBool "Not all left moves are implemented correctly." 
      $ flip all [North, South, East, West] $ \d -> do
        leftOf d == let Just x = lookup d (zip [North, East, South, West] [West, North, East, South]) in x
  , testCase "right behaves correctly"
      $ assertBool "Not all right moves are implemented correctly." 
      $ flip all [North, South, East, West] $ \d -> do
        leftOf d == let Just x = lookup d (zip [North, East, South, West] [West, North, East, South]) in x
  , testCase "two lefts and two rights are equivalent"
      $ assertBool "leftOf and rightOf are not inverses."
      $ flip all [North, South, East, West] $ \d -> do
        leftOf (leftOf d) == rightOf (rightOf d)
  ]

testStep :: TestTree
testStep = testGroup "Ex. 8: step"
  [ testCase "Is correct after a single step"
      $ hash (pack $ show $ step initialState) 
      @?= "\SYN\172F\159B\130:\231\145\142H\SO\227Z]\237"
  , testCase "Is correct after 10,000 steps"
      $ hash (pack $ show $ let i x =  x : i (step x) in i initialState !! 10000)
      @?= "\197\200\158\199+\174C#(\150k\219\248\221\170f"
  ]
