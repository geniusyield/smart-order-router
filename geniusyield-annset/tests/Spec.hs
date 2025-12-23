{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

import Properties
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    localOption (QuickCheckTests 10000) $
      testGroup
        "set-properties"
        [ testProperty "prop_Valid" prop_Valid
        , testProperty "prop_Single" prop_Single
        , testProperty "prop_Member" prop_Member
        , testProperty "prop_NotMember" prop_NotMember
        , testProperty "prop_Link" prop_Link
        , testProperty "prop_Merge" prop_Merge
        , testProperty "prop_size" prop_size
        , testProperty "prop_lookupMax" prop_lookupMax
        , testProperty "prop_lookupMin" prop_lookupMin
        , testProperty "prop_findMax" prop_findMax
        , testProperty "prop_findMin" prop_findMin
        , testProperty "prop_ord" prop_ord
        , testProperty "prop_foldR" prop_foldR
        , testProperty "prop_foldR'" prop_foldR'
        , testProperty "prop_foldL" prop_foldL
        , testProperty "prop_foldL'" prop_foldL'
        , testProperty "prop_maxView" prop_maxView
        , testProperty "prop_minView" prop_minView
        , testProperty "prop_filter" prop_filter
        , testProperty "takeWhileAntitone" prop_takeWhileAntitone
        , testProperty "dropWhileAntitone" prop_dropWhileAntitone
        , testProperty "spanAntitone" prop_spanAntitone
        , -- #if __GLASGOW_HASKELL__ >= 806
          testProperty "strict foldr" prop_strictFoldr'
        , testProperty "strict foldr" prop_strictFoldl'
        -- #endif
        ]
