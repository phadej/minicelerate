{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main (main) where

import Data.Word             (Word64)
import Test.QuickCheck       ((===))
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.HUnit ()
import Test.Tasty.QuickCheck (testProperty)

import qualified Language.Haskell.TH as TH

import Minicelerate

spliced :: Word64 -> Word64
spliced = $$(splice example)

-- because GHC is buggy we need to fallback to untyped Template Haskell.
cspliced :: Word64 -> Word64
cspliced = $(TH.unTypeCode (nativeCode example))

cspliced2 :: Word64 -> Word64
cspliced2 = $(TH.unTypeCode (nativeCodeManual example))

main :: IO ()
main = do
    runtime <- nativeCodeRuntime example

    defaultMain $ testGroup "minicelerate"
        [ testProperty "TH" $ \x ->
            interpret example x === spliced x

        , testProperty "NativeTH" $ \x ->
            interpret example x === cspliced x

        , testProperty "NativeTHManual" $ \x ->
            interpret example x === cspliced2 x

        , testProperty "Runtime" $ \x ->
            interpret example x === runtime x
        ]
