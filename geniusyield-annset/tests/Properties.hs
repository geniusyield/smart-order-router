{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Properties where

import Control.Monad
import Data.Foldable (all, toList)
import qualified Data.List as List
import Data.Maybe
import GHC.Natural
import GeniusYield.AnnSet.Internal
import GeniusYield.OrderBot.Types
import NoThunks.Class
import Test.QuickCheck
import Prelude hiding (all, drop, filter, foldl, foldr, lookup, map, null, splitAt, take)

type TestAnnSet = AnnSet Volume Natural

{--------------------------------------------------------------------
  Valid trees
--------------------------------------------------------------------}
forValid :: Testable b => (AnnSet Volume Natural -> b) -> Property
forValid f = forAll arbitrary $ \t ->
  classify (size t == 0) "empty" $
    classify (size t > 0 && size t <= 10) "small" $
      classify (size t > 10 && size t <= 64) "medium" $
        classify (size t > 64) "large" $
          f t

forValidUnitTree :: Testable a => (AnnSet Volume Natural -> a) -> Property
forValidUnitTree = forValid

prop_Valid :: Property
prop_Valid = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Member, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Natural -> Bool
prop_Single x = insert x empty == singleton x

prop_Member :: [Natural] -> Natural -> Bool
prop_Member xs n =
  let m = fromList xs
   in all (\k -> k `member` m == (k `elem` xs)) (n : xs)

prop_NotMember :: [Natural] -> Natural -> Bool
prop_NotMember xs n =
  let m = fromList xs
   in all (\k -> k `notMember` m == (k `notElem` xs)) (n : xs)

{--------------------------------------------------------------------
  Balance
--------------------------------------------------------------------}
prop_Link :: Natural -> Property
prop_Link x = forValidUnitTree $ \t ->
  let (l, r) = split x t
   in valid (link x l r)

prop_Merge :: Natural -> Property
prop_Merge x = forValidUnitTree $ \t ->
  let (l, r) = split x t
   in valid (merge l r)

{--------------------------------------------------------------------
  Set operations are like NaturalSet operations
--------------------------------------------------------------------}

data TwoSets = TwoSets TestAnnSet TestAnnSet deriving Show

instance Arbitrary TwoSets where
  arbitrary = TwoSets <$> arbitrary <*> arbitrary

prop_size :: AnnSet Volume Natural -> Bool
prop_size s = size s == List.length (toList s)

prop_findMax :: AnnSet Volume Natural -> Property
prop_findMax s = not (isEmpty s) ==> findMax s == maximum (toList s)

prop_findMin :: AnnSet Volume Natural -> Property
prop_findMin s = not (isEmpty s) ==> findMin s == minimum (toList s)

prop_lookupMin :: AnnSet Volume Natural -> Property
prop_lookupMin m = lookupMin m === (fst <$> minView m)

prop_lookupMax :: AnnSet Volume Natural -> Property
prop_lookupMax m = lookupMax m === (fst <$> maxView m)

prop_ord :: TwoSets -> Bool
prop_ord (TwoSets s1 s2) = s1 `compare` s2 == toList s1 `compare` toList s2

-- prop_readShow :: AnnSet Volume Natural -> Bool
-- prop_readShow s = s == read (show s)

prop_foldR :: AnnSet Volume Natural -> Bool
prop_foldR s = foldrAnnSet (:) [] s == toList s

prop_foldR' :: AnnSet Volume Natural -> Bool
prop_foldR' s = foldr'AnnSet (:) [] s == toList s

prop_foldL :: AnnSet Volume Natural -> Bool
prop_foldL s = foldlAnnSet (flip (:)) [] s == List.foldl (flip (:)) [] (toList s)

prop_foldL' :: AnnSet Volume Natural -> Bool
prop_foldL' s = foldl'AnnSet (flip (:)) [] s == List.foldl' (flip (:)) [] (toList s)

-- prop_map :: AnnSet Volume Natural -> Bool
-- prop_map s = map id s == s

-- prop_map2 :: Fun Natural Natural -> Fun Natural Natural -> AnnSet Volume Natural -> Property
-- prop_map2 f g s = map (apply f) (map (apply g) s) === map (apply f . apply g) s

-- prop_mapMonotonic :: AnnSet Volume Natural -> Property
-- prop_mapMonotonic s = mapMonotonic id s === s

prop_maxView :: AnnSet Volume Natural -> Bool
prop_maxView s = case maxView s of
  Nothing -> isEmpty s
  Just (m, s') -> m == maximum (toList s) && s == insert m s' && m `notMember` s'

prop_minView :: AnnSet Volume Natural -> Bool
prop_minView s = case minView s of
  Nothing -> isEmpty s
  Just (m, s') -> m == minimum (toList s) && s == insert m s' && m `notMember` s'

-- prop_split :: AnnSet Volume Natural -> Natural -> Bool
-- prop_split s i = case split i s of
--    (s1,s2) -> all (<i) (toList s1) && all (>i) (toList s2) && i `delete` s == union s1 s2

-- prop_splitMember :: AnnSet Volume Natural -> Natural -> Bool
-- prop_splitMember s i = case splitMember i s of
--    (s1,t,s2) -> all (<i) (toList s1) && all (>i) (toList s2) && t == i `member` s && i `delete` s == union s1 s2

{- -prop_splitRoot :: AnnSet Volume Natural -> Bool
prop_splitRoot s = loop ls && (s == unions ls)
 where
  ls = splitRoot s
  loop [] = True
  loop (s1:rst) = List.null
                  [ (x,y) | x <- toList s1
                          , y <- toList (unions rst)
                          , x > y ]
prop_partition :: AnnSet Volume Natural -> Natural -> Bool
prop_partition s i = case partition odd s of
    (s1,s2) -> all odd (toList s1) && all even (toList s2) && s == s1 `union` s2
-}

prop_filter :: AnnSet Volume Natural -> Natural -> Bool
prop_filter s _ = partition odd s == (filter odd s, filter even s)

{-
prop_take :: Natural -> AnnSet Volume Natural -> Property
prop_take n xs = valid taken .&&.
                 taken === fromDistinctAscList (List.take n (toList xs))
  where
    taken = take n xs

prop_drop :: Natural -> AnnSet Volume Natural -> Property
prop_drop n xs = valid dropped .&&.
                 dropped === fromDistinctAscList (List.drop n (toList xs))
  where
    dropped = drop n xs

prop_splitAt :: Natural -> AnnSet Volume Natural -> Property
prop_splitAt n xs = valid taken .&&.
                    valid dropped .&&.
                    taken === take n xs .&&.
                    dropped === drop n xs
  where
    (taken, dropped) = splitAt n xs
-}
prop_takeWhileAntitone :: [Either Natural Natural] -> Property
prop_takeWhileAntitone xs' = valid tw .&&. tw === filter isLeft xs
 where
  xs = fromList xs'
  tw = takeWhileAntitone isLeft xs

prop_dropWhileAntitone :: [Either Natural Natural] -> Property
prop_dropWhileAntitone xs' = valid tw .&&. tw === filter (not . isLeft) xs
 where
  xs = fromList xs'
  tw = dropWhileAntitone isLeft xs

prop_spanAntitone :: [Either Natural Natural] -> Property
prop_spanAntitone xs' =
  valid tw
    .&&. valid dw
    .&&. tw === takeWhileAntitone isLeft xs
    .&&. dw === dropWhileAntitone isLeft xs
 where
  xs = fromList xs'
  (tw, dw) = spanAntitone isLeft xs

{-
prop_powerSet :: AnnSet Volume Natural -> Property
prop_powerSet xs = valid ps .&&. ps === ps'
  where
    xs' = take 10 xs

    ps = powerSet xs'
    ps' = fromList . fmap fromList $ lps (toList xs')

    lps [] = [[]]
    lps (y : ys) = fmap (y:) (lps ys) ++ lps ys

prop_cartesianProduct :: AnnSet Volume Natural -> AnnSet Volume Natural -> Property
prop_cartesianProduct xs ys =
  valid cp .&&. toList cp === liftA2 (,) (toList xs) (toList ys)
  where cp = cartesianProduct xs ys

prop_disjointUnion :: AnnSet Volume Natural -> AnnSet Volume Natural -> Property
prop_disjointUnion xs ys =
  valid du .&&. du === union (mapMonotonic Left xs) (mapMonotonic Right ys)
  where du = disjointUnion xs ys
-}
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

#if __GLASGOW_HASKELL__ >= 806
prop_strictFoldr' :: AnnSet Volume Natural -> Property
prop_strictFoldr' m = whnfHasNoThunks (foldr'AnnSet (:) [] m)
#endif

#if __GLASGOW_HASKELL__ >= 806
prop_strictFoldl' :: AnnSet Volume Natural -> Property
prop_strictFoldl' m = whnfHasNoThunks (foldl'AnnSet (flip (:)) [] m)
#endif

{- | Check that after evaluating the argument to weak head normal form there
 are no thunks.
-}
whnfHasNoThunks :: NoThunks a => a -> Property
whnfHasNoThunks a =
  ioProperty
    . fmap isNothing
    . noThunks []
    $! a
