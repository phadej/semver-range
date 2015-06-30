module Main (main) where

import Control.Applicative
import Data.Maybe
import Data.SemVer.Range
import Test.Tasty
import Test.Tasty.QuickCheck as QC

infixr 0 `to`

to :: a -> b -> (a, b)
to a b = (a, b)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" 
  [ exampleVersionProps
  , invalidVersionProps
  , exampleRangeProps
  , simpleRangeProps
  , advancedRangeProps
  ]

exampleVersionProps :: TestTree
exampleVersionProps = testGroup "version examples" $ mk <$> exampleVersions
  where mk (name, v) = testProperty name $ once $ parseVersion name === Just v

invalidVersionProps :: TestTree
invalidVersionProps = testGroup "invalid version examples" $ mk <$> invalidVersions
  where mk name = testProperty name $ once $ isNothing $ parseVersion name

exampleRangeProps :: TestTree
exampleRangeProps = testGroup "range examples" $ mk <$> exampleRanges
  where mk (name, v) = testProperty name $ once $ parseVersionRange name === Just v

simpleRangeProps :: TestTree
simpleRangeProps = testGroup "succeed parsing simple ranges" $ mk <$> advancedRanges
  where mk (_, simple) = testProperty simple $ once $ isJust $ parseVersionRange simple

advancedRangeProps :: TestTree
advancedRangeProps = testGroup "advanced range examples" $ mk <$> advancedRanges
  where mk (advanced, simple) = testProperty advanced $ once $ parseVersionRange advanced === parseVersionRange simple

exampleVersions :: [(String, Version)]
exampleVersions =
  [ "1.2.3" `to` version 1 2 3 
  , "1.2.3-beta.4" `to` Version 1 2 3 [IStr "beta", INum 4]
  ]

invalidVersions :: [String]
invalidVersions = 
  [ "1.2"
  , "a.b.c"
  ]

exampleRanges :: [(String, VersionRange)]
exampleRanges =
  [ ">=1.2.7" `to` range ROGE (version 1 2 7)
  , "<1.3.0" `to` range ROLT (version 1 3 0)
  , ">=1.2.7 <1.3.0" `to` range ROGE (version 1 2 7) /\ range ROLT (version 1 3 0)
  , "1.2.7 || >=1.2.9 <2.0.0" `to` range ROEQ (version 1 2 7) \/ (range ROGE (version 1 2 9) /\ range ROLT (version 2 0 0))
  ]
  
advancedRanges :: [(String, String)]
advancedRanges =
  [ "1.2.3 - 2.3.4" `to` ">=1.2.3 <=2.3.4"
  , "1.2 - 2.3.4" `to` ">=1.2.0 <=2.3.4"
  , "1.2.3 - 2.3" `to` ">=1.2.3 <2.4.0"
  , "1.2.3 - 2" `to` ">=1.2.3 <3.0.0"
  -- X-Ranges
  , "*" `to` ">=0.0.0"
  , "1.x" `to` ">=1.0.0 <2.0.0"
  , "1.2.x" `to` ">=1.2.0 <1.3.0"
  -- Partial range
  , "" `to` ">=0.0.0"
  , "1" `to` ">=1.0.0 <2.0.0"
  , "1.x.x" `to` ">=1.0.0 <2.0.0"
  , "1.2" `to` ">=1.2.0 <1.3.0"
  -- Tilde ranges
  , "~1.2.3" `to` ">=1.2.3 <1.3.0"
  , "~1.2" `to` ">=1.2.0 <1.3.0"
  , "~1" `to` ">=1.0.0 <2.0.0"
  , "~0.2.3" `to` ">=0.2.3 <0.3.0"
  , "~0.2" `to` ">=0.2.0 <0.3.0"
  , "~0" `to` ">=0.0.0 <1.0.0"
  , "~1.2.3-beta.2" `to` ">=1.2.3-beta.2 <1.3.0"
  -- Caret ranges
  , "^1.2.3" `to` ">=1.2.3 <2.0.0"
  , "^0.2.3" `to` ">=0.2.3 <0.3.0"
  , "^0.0.3" `to` ">=0.0.3 <0.0.4"
  , "^1.2.3-beta.2" `to` ">=1.2.3-beta.2 <2.0.0"
  , "^0.0.3-beta" `to` ">=0.0.3-beta <0.0.4"
  -- missing patch
  , "^1.2.x" `to` ">=1.2.0 <2.0.0"
  , "^0.0.x" `to` ">=0.0.0 <0.1.0"
  , "^0.0"   `to` ">=0.0.0 <0.1.0"
  -- missing minor
  , "^1.x" `to` ">=1.0.0 <2.0.0"
  , "^0.x" `to` ">=0.0.0 <1.0.0"
  ]

