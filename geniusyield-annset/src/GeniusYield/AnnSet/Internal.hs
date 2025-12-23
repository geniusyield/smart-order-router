{-# LANGUAGE CPP #-}

module GeniusYield.AnnSet.Internal (
  AnnSet (..),
  size,
  fromList,
  toAscList,
  empty,
  takeWhileAntitone,
  dropWhileAntitone,
  member,
  singleton,
  insert,
  delete,
  lookupMin,
  lookupMax,
  findMin,
  findMax,
  link,
  glue,
  merge,
  filter,
  filterWithMeasure,
  partition,
  valid,
  module GeniusYield.AnnSet.Types,
  -- specialized functions/structures for limit order book
  Orders,
  SellOrders,
  BuyOrders,
  mkOrderView,
  mkOrders,
  -- for tests
  foldrAnnSet,
  foldr'AnnSet,
  foldlAnnSet,
  foldl'AnnSet,
  foldl'WithMeasure,
  spanAntitone,
  notMember,
  isEmpty,
  minView,
  maxView,
  split,
) where

import Data.Foldable
import Data.Kind
import Data.Ord
import GeniusYield.AnnSet.Types
import GeniusYield.AnnSet.Unsafe
import GeniusYield.OrderBot.Types
import Test.QuickCheck.Arbitrary
import Utils.Containers.Internal.StrictPair
import Prelude hiding (filter, map)

type Size = Int

{- | A binary tree that represents a Set annotated with an extra monoid field.

   Most of the operations require that @a@ be an instance of @Ord@ and that
   a @Measured v a@ instance exists.

   The general idea is that we use the monoid annotation @v@ to store auxiliary information about
   the elements of the tree. Because that information must be in the form of a monoid,
   it allows us to have very fast (i.e. O(1)) access to the monoidal summary of the annotation
   information for the whole set

   The summary represented by the annotation is recalculated (though not "from scratch")
   during rebalancing operations. All other functions are equivalent to the Data.Set functions
   of the same name (in terms of complexity and )
-}
data AnnSet (v :: Type) (a :: Type)
  = Entry !v {-# UNPACK #-} !Size !a !(AnnSet v a) !(AnnSet v a)
  | Empty

instance (Arbitrary a, Ord a, Measured a, MeasureOut a ~ v) => Arbitrary (AnnSet v a) where
  arbitrary = fromList <$> (arbitrary @[a])

instance Monoid v => Measured (AnnSet v a) where
  type MeasureOut (AnnSet v a) = v
  measure Empty = mempty
  measure (Entry v _ _ _ _) = v

instance Show a => Show (AnnSet v a) where
  show bk = "fromList " <> show (toList bk)

instance Eq a => Eq (AnnSet v a) where
  t1 == t2 = (size t1 == size t2) && (toList t1 == toList t2)

instance Ord a => Ord (AnnSet v a) where
  compare b1 b2 = compare (toList b1) (toList b2)

-- | Folds in order of increasing key.
instance Foldable (AnnSet v) where
  fold = go
   where
    go Empty = mempty
    go (Entry _ 1 k _ _) = k
    go (Entry _ _ k l r) = go l `mappend` (k `mappend` go r)
  {-# INLINEABLE fold #-}
  foldr = foldrAnnSet
  {-# INLINE foldr #-}
  foldl = foldlAnnSet
  {-# INLINE foldl #-}
  foldMap f = go
   where
    go Empty = mempty
    go (Entry _ 1 k _ _) = f k
    go (Entry _ _ k l r) = go l `mappend` (f k `mappend` go r)
  {-# INLINE foldMap #-}
  foldl' = foldl'AnnSet
  {-# INLINE foldl' #-}
  foldr' = foldr'AnnSet
  {-# INLINE foldr' #-}
  length = size
  {-# INLINE length #-}
  null = isEmpty
  {-# INLINE null #-}
  toList = foldrAnnSet (:) []
  {-# INLINE toList #-}
  elem = go
   where
    go !_ Empty = False
    go x (Entry _ _ y l r) = x == y || go x l || go x r
  {-# INLINEABLE elem #-}
  minimum = findMin
  {-# INLINE minimum #-}
  maximum = findMax
  {-# INLINE maximum #-}
  sum = foldl'AnnSet (+) 0
  {-# INLINEABLE sum #-}
  product = foldl'AnnSet (*) 1
  {-# INLINEABLE product #-}

size :: AnnSet v a -> Int
size Empty = 0
size (Entry _ sz _ _ _) = sz
{-# INLINE size #-}

isEmpty :: AnnSet v a -> Bool
isEmpty Empty = True
isEmpty Entry {} = False
{-# INLINE isEmpty #-}

-- | \(O(\log n)\). Is the element in the set?
member :: (Ord a, Measured a) => a -> AnnSet v a -> Bool
member = go
 where
  go !_ Empty = False
  go x (Entry _ _ y l r) = case compare x y of
    LT -> go x l
    GT -> go x r
    EQ -> True

#if __GLASGOW_HASKELL__
{-# INLINABLE member #-}
#else
{-# INLINE member #-}
#endif

-- | \(O(\log n)\). Is the element not in the set?
notMember :: (Ord a, Measured a) => a -> AnnSet (MeasureOut a) a -> Bool
notMember a t = not $ member a t

#if __GLASGOW_HASKELL__
{-# INLINABLE notMember #-}
#else
{-# INLINE notMember #-}
#endif

empty :: AnnSet v a
empty = Empty

singleton :: Measured a => a -> AnnSet (MeasureOut a) a
singleton a = Entry (measure a) 1 a Empty Empty

delta, ratio :: Int
delta = 3
ratio = 2

-- | \(O(\log n)\). Delete an element from a set.

-- See Note: Type of local 'go' function
delete :: forall v a. (Ord a, Measured a, MeasureOut a ~ v) => a -> AnnSet v a -> AnnSet v a
delete = go
 where
  go :: a -> AnnSet v a -> AnnSet v a
  go !_ Empty = Empty
  go x t@(Entry _ _ y l r) = case compare x y of
    LT
      | l' `ptrEq` l -> t
      | otherwise -> balanceR y l' r
     where
      !l' = go x l
    GT
      | r' `ptrEq` r -> t
      | otherwise -> balanceL y l r'
     where
      !r' = go x r
    EQ -> glue l r
{-# INLINEABLE delete #-}

{-# INLINEABLE insert #-}
insert :: forall a. (Ord a, Measured a) => a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
insert x0 = go x0 x0
 where
  go :: a -> a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
  go orig !_ Empty = singleton (lazy orig)
  go orig !x t@(Entry v sz y l r) = case compare x y of
    LT
      | l' `ptrEq` l -> t
      | otherwise -> balanceL y l' r
     where
      !l' = go orig x l
    GT
      | r' `ptrEq` r -> t
      | otherwise -> balanceR y l r'
     where
      !r' = go orig x r
    EQ
      | lazy orig `seq` (orig `ptrEq` y) -> t
      | otherwise -> Entry v sz (lazy orig) l r

lazy :: a -> a
lazy a = a

balanceL :: forall a. Measured a => a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
balanceL x l r = case r of
  Empty -> case l of
    Empty -> Entry (measure x) 1 x Empty Empty
    (Entry ml _ _ Empty Empty) -> Entry (measure x <> ml) 2 x l Empty
    (Entry _ _ lx Empty (Entry _ _ lrx _ _)) ->
      let !mlrx = measure lrx
          !mlx = measure lx
       in Entry
            (mconcat [mlrx, mlx, mx])
            3
            lrx
            (Entry mlx 1 lx Empty Empty)
            (Entry mx 1 x Empty Empty)
    (Entry _ _ lx ll@(Entry mll _ _ _ _) Empty) ->
      Entry
        (mconcat [mx, measure lx, mll])
        3
        lx
        ll
        (Entry mx 1 x Empty Empty)
    (Entry _ ls lx ll@(Entry mll lls _ _ _) lr@(Entry mlr lrs lrx lrl lrr))
      | lrs < ratio * lls ->
          Entry
            (measure lx <> mll <> mx <> mlr)
            (1 + ls)
            lx
            ll
            (Entry (mx <> mlr) (1 + lrs) x lr Empty)
      | otherwise ->
          let !vlc = measure lx <> mll <> measure lrl
              !vrc = mx <> measure lrr
           in Entry
                (measure lrx <> vlc <> vrc)
                (1 + ls)
                lrx
                (Entry vlc (1 + lls + size lrl) lx ll lrl)
                (Entry vrc (1 + size lrr) x lrr Empty)
  (Entry mr rs _ _ _) -> case l of
    Empty -> Entry (mx <> mr) (1 + rs) x Empty r
    (Entry ml ls lx ll lr)
      | ls > delta * rs -> case (ll, lr) of
          (Entry mll lls _ _ _, Entry mlr lrs lrx lrl lrr)
            | lrs < ratio * lls ->
                let !v = mx <> mlr <> mr
                 in Entry (measure lx <> mll <> v) (1 + ls + rs) lx ll (Entry v (1 + rs + lrs) x lr r)
            | otherwise ->
                let !vlc = measure lx <> mll <> measure lrl
                    !vrc = mx <> measure lrr <> mr
                 in Entry
                      (measure lrx <> vlc <> vrc)
                      (1 + ls + rs)
                      lrx
                      (Entry vlc (1 + lls + size lrl) lx ll lrl)
                      (Entry vrc (1 + rs + size lrr) x lrr r)
          (_, _) -> error "Failure in Data.Map.balanceL"
      | otherwise -> Entry (mx <> ml <> mr) (1 + ls + rs) x l r
 where
  !mx = measure x
{-# NOINLINE balanceL #-}

-- balanceR is called when right subtree might have been inserted to or when
-- left subtree might have been deleted from.
balanceR :: forall a. Measured a => a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
balanceR x l r = case l of
  Empty -> case r of
    Empty -> Entry (measure x) 1 x Empty Empty
    (Entry _ _ _ Empty Empty) -> Entry (measure x <> measure r) 2 x Empty r
    (Entry _ _ rx Empty rr@Entry {}) ->
      Entry
        (measure rx <> measure x <> measure rr)
        3
        rx
        (Entry (measure x) 1 x Empty Empty)
        rr
    (Entry _ _ rx (Entry _ _ rlx _ _) Empty) ->
      Entry
        (measure rlx <> measure x <> measure rx)
        3
        rlx
        (Entry (measure x) 1 x Empty Empty)
        (Entry (measure rx) 1 rx Empty Empty)
    (Entry _ rs rx rl@(Entry _ rls rlx rll rlr) rr@(Entry _ rrs _ _ _))
      | rls < ratio * rrs ->
          Entry
            (measure rx <> measure x <> measure rl <> measure rr)
            (1 + rs)
            rx
            (Entry (measure x <> measure rl) (1 + rls) x Empty rl)
            rr
      | otherwise ->
          Entry
            (measure rlx <> measure x <> measure rll <> measure rx <> measure rlr <> measure rr)
            (1 + rs)
            rlx
            (Entry (measure x <> measure rll) (1 + size rll) x Empty rll)
            (Entry (measure rx <> measure rlr <> measure rr) (1 + rrs + size rlr) rx rlr rr)
  (Entry _ ls _ _ _) -> case r of
    Empty -> Entry (measure x <> measure l) (1 + ls) x l Empty
    (Entry _ rs rx rl rr)
      | rs > delta * ls -> case (rl, rr) of
          (Entry _ rls rlx rll rlr, Entry _ rrs _ _ _)
            | rls < ratio * rrs ->
                let !vlc = measure x <> measure l <> measure rl
                    !vrc = measure rr
                 in Entry
                      (measure rx <> vlc <> vrc)
                      (1 + ls + rs)
                      rx
                      (Entry vlc (1 + ls + rls) x l rl)
                      rr
            | otherwise ->
                Entry
                  (measure rlx <> measure x <> measure l <> measure rll <> measure rx <> measure rlr <> measure rr)
                  (1 + ls + rs)
                  rlx
                  (Entry (measure x <> measure l <> measure rll) (1 + ls + size rll) x l rll)
                  (Entry (measure rx <> measure rlr <> measure rr) (1 + rrs + size rlr) rx rlr rr)
          (_, _) -> error "Failure in Data.Map.balanceR"
      | otherwise -> Entry (measure x <> measure l <> measure r) (1 + ls + rs) x l r
{-# NOINLINE balanceR #-}

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

{- | \(O(n)\). Fold the elements in the set using the given right-associative
 binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.

 For example,

 > toAscList set = foldr (:) [] set
-}
foldrAnnSet :: (a -> b -> b) -> b -> AnnSet v a -> b
foldrAnnSet f = go
 where
  go z' Empty = z'
  go z' (Entry _ _ x l r) = go (f x (go z' r)) l
{-# INLINE foldrAnnSet #-}

{- | \(O(n)\). A strict version of 'foldr'. Each application of the operator is
 evaluated before using the result in the next application. This
 function is strict in the starting value.
-}
foldr'AnnSet :: (a -> b -> b) -> b -> AnnSet v a -> b
foldr'AnnSet f = go
 where
  go !z' Empty = z'
  go z' (Entry _ _ x l r) = go (f x $! go z' r) l
{-# INLINE foldr'AnnSet #-}

{- | \(O(n)\). Fold the elements in the set using the given left-associative
 binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.

 For example,

 > toDescList set = foldl (flip (:)) [] set
-}
foldlAnnSet :: (a -> b -> a) -> a -> AnnSet v b -> a
foldlAnnSet f = go
 where
  go z' Empty = z'
  go z' (Entry _ _ x l r) = go (f (go z' l) x) r
{-# INLINE foldlAnnSet #-}

{- | \(O(n)\). A strict version of 'foldl'. Each application of the operator is
 evaluated before using the result in the next application. This
 function is strict in the starting value.
-}
foldl'AnnSet :: (a -> b -> a) -> a -> AnnSet v b -> a
foldl'AnnSet f = go
 where
  go !z' Empty = z'
  go z' (Entry _ _ x l r) =
    let !z'' = go z' l
     in go (f z'' x) r
{-# INLINE foldl'AnnSet #-}

foldl'WithMeasure :: Measured b => a -> AnnSet (MeasureOut b) b -> (a -> MeasureOut b -> b -> a) -> a
foldl'WithMeasure z s f = go z s
 where
  go !z' Empty = z'
  go z' (Entry _ _ x l r) =
    let !z'' = go z' l
        !m = measure x
     in go (f z'' m x) r

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- We perform call-pattern specialization manually on lookupMin
-- and lookupMax. Otherwise, GHC doesn't seem to do it, which is
-- unfortunate if, for example, someone uses findMin or findMax.

lookupMinSure :: a -> AnnSet v a -> a
lookupMinSure x Empty = x
lookupMinSure _ (Entry _ _ x l _) = lookupMinSure x l

{- | \(O(\log n)\). The minimal element of a set.

 @since 0.5.9
-}
lookupMin :: AnnSet v a -> Maybe a
lookupMin Empty = Nothing
lookupMin (Entry _ _ x l _) = Just $! lookupMinSure x l

-- | \(O(\log n)\). The minimal element of a set.
findMin :: AnnSet v a -> a
findMin t
  | Just r <- lookupMin t = r
  | otherwise = error "AnnSet.findMin: empty set has no minimal element"

lookupMaxSure :: a -> AnnSet v a -> a
lookupMaxSure x Empty = x
lookupMaxSure _ (Entry _ _ x _ r) = lookupMaxSure x r

{- | \(O(\log n)\). The maximal element of a set.

 @since 0.5.9
-}
lookupMax :: AnnSet v a -> Maybe a
lookupMax Empty = Nothing
lookupMax (Entry _ _ x _ r) = Just $! lookupMaxSure x r

-- | \(O(\log n)\). The maximal element of a set.
findMax :: AnnSet v a -> a
findMax t
  | Just r <- lookupMax t = r
  | otherwise = error "AnnSet.findMax: empty set has no maximal element"

-- bad implementation, just for testing something
fromList :: forall a. (Ord a, Measured a) => [a] -> AnnSet (MeasureOut a) a
fromList = foldl' (flip insert) empty

toAscList :: AnnSet v a -> [a]
toAscList = foldr (:) []

{-
-- | \(O(n \log n)\). Create a set from a list of elements.
--
-- If the elements are ordered, a linear-time implementation is used,
-- with the performance equal to 'fromDistinctAscList'.

-- For some reason, when 'singleton' is used in fromList or in
-- create, it is not inlined, so we inline it manually.
fromList :: forall v a. (Ord a, Measured v a) => [a] -> AnnSet (MeasureOut a) a
fromList [] = Empty
fromList [x] = Entry (measure x) 1 x Empty Empty
fromList (x0 : xs0) | not_ordered x0 xs0 = fromList' (Entry (measure x0) 1 x0 Empty Empty) xs0
                    | otherwise = go (1::Int) (Entry (measure x0) 1 x0 Empty Empty) xs0
  where
    not_ordered _ [] = False
    not_ordered x (y : _) = x >= y
    {-# INLINE not_ordered #-}

    fromList' t0 xs = foldl' ins t0 xs
      where ins t x = insert x t

    go :: Size -> AnnSet (MeasureOut a) a -> [a] -> AnnSet (MeasureOut a) a
    go !_ t [] = t
    go _ t [x] = insertMax x t
    go s l xs@(x : xss) | not_ordered x xss = fromList' l xs
                        | otherwise = case create s xss of
                            (r, ys, []) -> go (s `shiftL` 1) (link x l r) ys
                            (r, _,  ys) -> fromList' (link x l r) ys

    -- The create is returning a triple (tree, xs, ys). Both xs and ys
    -- represent not yet processed elements and only one of them can be nonempty.
    -- If ys is nonempty, the keys in ys are not ordered with respect to tree
    -- and must be inserted using fromList'. Otherwise the keys have been
    -- ordered so far.
    create :: Size -> [a] -> (AnnSet (MeasureOut a) a, [a], [a])
    create !_ [] = (Empty, [], [])
    create s xs@(x : xss)
      | s == 1 = if not_ordered x xss then (Entry (measure x) 1 x Empty Empty, [], xss)
                                      else (Entry (measure x) 1 x Empty Empty, xss, [])
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, [], _) -> res
                      (l, [y], zs) -> (insertMax y l, [], zs)
                      (l, ys@(y:yss), _) | not_ordered y yss -> (l, [], ys)
                                         | otherwise -> case create (s `shiftR` 1) yss of
                                                   (r, zs, ws) -> (link y l r, zs, ws)
{-# INLINABLE fromList #-}
-}

{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}
link :: Measured a => a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
link x Empty r = insertMin x r
link x l Empty = insertMax x l
link x l@(Entry _ sizeL y ly ry) r@(Entry _ sizeR z lz rz)
  | delta * sizeL < sizeR = balanceL z (link x l lz) rz
  | delta * sizeR < sizeL = balanceR y ly (link x ry r)
  | otherwise = bin x l r

-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax, insertMin :: Measured a => a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
insertMax x t =
  case t of
    Empty -> singleton x
    Entry _ _ y l r ->
      balanceR y l (insertMax x r)
insertMin x t =
  case t of
    Empty -> singleton x
    Entry _ _ y l r ->
      balanceL y (insertMin x l) r

{- | \(O(\log n)\). Take while a predicate on the elements holds.
 The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
 @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.

 @
 takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' p . 'toList'
 takeWhileAntitone p = 'filter' p
 @

 @since 0.5.8
-}
takeWhileAntitone :: Measured a => (a -> Bool) -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
takeWhileAntitone _ Empty = Empty
takeWhileAntitone p (Entry _ _ x l r)
  | p x = link x l (takeWhileAntitone p r)
  | otherwise = takeWhileAntitone p l

{- | \(O(\log n)\). Drop while a predicate on the elements holds.
 The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
 @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.

 @
 dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' p . 'toList'
 dropWhileAntitone p = 'filter' (not . p)
 @

 @since 0.5.8
-}
dropWhileAntitone :: Measured a => (a -> Bool) -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
dropWhileAntitone _ Empty = Empty
dropWhileAntitone p (Entry _ _ x l r)
  | p x = dropWhileAntitone p r
  | otherwise = link x (dropWhileAntitone p l) r

{- | \(O(\log n)\). Divide a set at the point where a predicate on the elements stops holding.
 The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
 @j \< k ==\> p j \>= p k@.

 @
 spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
 spanAntitone p xs = partition p xs
 @

 Note: if @p@ is not actually antitone, then @spanAntitone@ will split the set
 at some /unspecified/ point where the predicate switches from holding to not
 holding (where the predicate is seen to hold before the first element and to fail
 after the last element).

 @since 0.5.8
-}
spanAntitone :: Measured a => (a -> Bool) -> AnnSet (MeasureOut a) a -> (AnnSet (MeasureOut a) a, AnnSet (MeasureOut a) a)
spanAntitone p0 m = toPair (go p0 m)
 where
  go _ Empty = Empty :*: Empty
  go p (Entry _ _ x l r)
    | p x = let u :*: v = go p r in link x l u :*: v
    | otherwise = let u :*: v = go p l in u :*: link x v r

{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Measured a => AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
merge Empty r = r
merge l Empty = l
merge l@(Entry _ sizeL x lx rx) r@(Entry _ sizeR y ly ry)
  | delta * sizeL < sizeR = balanceL y (merge l ly) ry
  | delta * sizeR < sizeL = balanceR x lx (merge rx r)
  | otherwise = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Measured a => AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
glue Empty r = r
glue l Empty = l
glue l@(Entry _ sl xl ll lr) r@(Entry _ sr xr rl rr)
  | sl > sr = let !(m :*: l') = maxViewSure xl ll lr in balanceR m l' r
  | otherwise = let !(m :*: r') = minViewSure xr rl rr in balanceL m l r'

minViewSure :: Measured a => a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a -> StrictPair a (AnnSet (MeasureOut a) a)
minViewSure = go
 where
  go x Empty r = x :*: r
  go x (Entry _ _ xl ll lr) r =
    case go xl ll lr of
      xm :*: l' -> xm :*: balanceR x l' r

{- | \(O(\log n)\). Retrieves the minimal key of the set, and the set
 stripped of that element, or 'Nothing' if passed an empty set.
-}
minView :: Measured a => AnnSet (MeasureOut a) a -> Maybe (a, AnnSet (MeasureOut a) a)
minView Empty = Nothing
minView (Entry _ _ x l r) = Just $! toPair $ minViewSure x l r

maxViewSure :: Measured a => a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a -> StrictPair a (AnnSet (MeasureOut a) a)
maxViewSure = go
 where
  go x l Empty = x :*: l
  go x l (Entry _ _ xr rl rr) =
    case go xr rl rr of
      xm :*: r' -> xm :*: balanceL x l r'

{- | \(O(\log n)\). Retrieves the maximal key of the set, and the set
 stripped of that element, or 'Nothing' if passed an empty set.
-}
maxView :: Measured a => AnnSet (MeasureOut a) a -> Maybe (a, AnnSet (MeasureOut a) a)
maxView Empty = Nothing
maxView (Entry _ _ x l r) = Just $! toPair $ maxViewSure x l r

{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: Measured a => a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
bin x l r =
  Entry (measure x <> measure l <> measure r) (size l + size r + 1) x l r
{-# INLINE bin #-}

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}

{- | \(O(\log n)\). The expression (@'split' x set@) is a pair @(set1,set2)@
 where @set1@ comprises the elements of @set@ less than @x@ and @set2@
 comprises the elements of @set@ greater than @x@.
-}
split :: (Ord a, Measured a) => a -> AnnSet (MeasureOut a) a -> (AnnSet (MeasureOut a) a, AnnSet (MeasureOut a) a)
split x t = toPair $ splitS x t
{-# INLINEABLE split #-}

splitS :: (Ord a, Measured a) => a -> AnnSet (MeasureOut a) a -> StrictPair (AnnSet (MeasureOut a) a) (AnnSet (MeasureOut a) a)
splitS _ Empty = Empty :*: Empty
splitS x (Entry _ _ y l r) =
  case compare x y of
    LT -> let (lt :*: gt) = splitS x l in (lt :*: link y gt r)
    GT -> let (lt :*: gt) = splitS x r in (link y l lt :*: gt)
    EQ -> l :*: r
{-# INLINEABLE splitS #-}

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}

-- | \(O(n)\). Filter all elements that satisfy the predicate.
filter :: Measured a => (a -> Bool) -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
filter _ Empty = Empty
filter p t@(Entry _ _ x l r)
  | p x =
      if l `ptrEq` l' && r `ptrEq` r'
        then t
        else link x l' r'
  | otherwise = merge l' r'
 where
  !l' = filter p l
  !r' = filter p r

-- | \(O(n)\)
filterWithMeasure :: Measured a => (MeasureOut a -> a -> Bool) -> AnnSet (MeasureOut a) a -> AnnSet (MeasureOut a) a
filterWithMeasure _ Empty = Empty
filterWithMeasure p t@(Entry _ _ x l r)
  | p (measure x) x =
      if l `ptrEq` l' && r `ptrEq` r'
        then t
        else link x l' r'
  | otherwise = merge l' r'
 where
  !l' = filterWithMeasure p l
  !r' = filterWithMeasure p r

{- | \(O(n)\). Partition the set into two sets, one with all elements that satisfy
 the predicate and one with all elements that don't satisfy the predicate.
 See also 'split'.
-}
partition :: Measured a => (a -> Bool) -> AnnSet (MeasureOut a) a -> (AnnSet (MeasureOut a) a, AnnSet (MeasureOut a) a)
partition p0 t0 = toPair $ go p0 t0
 where
  go _ Empty = Empty :*: Empty
  go p t@(Entry _ _ x l r) = case (go p l, go p r) of
    (l1 :*: l2, r1 :*: r2)
      | p x ->
          ( if l1 `ptrEq` l && r1 `ptrEq` r
              then t
              else link x l1 r1
          )
            :*: merge l2 r2
      | otherwise ->
          merge l1 r1
            :*: ( if l2 `ptrEq` l && r2 `ptrEq` r
                    then t
                    else link x l2 r2
                )

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}

-- | \(O(n)\). Test if the internal set structure is valid.
valid :: (Ord a, Eq (MeasureOut a), Measured a) => AnnSet (MeasureOut a) a -> Bool
valid t =
  balanced t && ordered t && validsize t && validMeasure t

ordered :: Ord a => AnnSet v a -> Bool
ordered = bounded (const True) (const True)
 where
  bounded lo hi t' =
    case t' of
      Empty -> True
      Entry _ _ x l r -> lo x && hi x && bounded lo (< x) l && bounded (> x) hi r

balanced :: AnnSet v a -> Bool
balanced t =
  case t of
    Empty -> True
    Entry _ _ _ l r ->
      (size l + size r <= 1 || (size l <= delta * size r && size r <= delta * size l))
        && balanced l
        && balanced r

validsize :: AnnSet v a -> Bool
validsize t = realsize t == Just (size t)
 where
  realsize t' =
    case t' of
      Empty -> Just 0
      Entry _ sz _ l r -> case (realsize l, realsize r) of
        (Just n, Just m) | n + m + 1 == sz -> Just sz
        _ -> Nothing

validMeasure :: (Eq (MeasureOut a), Measured a) => AnnSet (MeasureOut a) a -> Bool
validMeasure bk = measure bk == slowMeasure bk
 where
  slowMeasure :: Measured a => AnnSet (MeasureOut a) a -> MeasureOut a
  slowMeasure = foldl'AnnSet (\acc x -> acc <> measure x) mempty

-- Specialized functions and data structures for the GY implementation of the order book.
-- Morally these shouldn't be in this module but practical concerns dictate that they be
-- placed here

-- The (b :: Bias) type argument to OrderView lets us traverse the books in the most "interesting"
-- order (low -> high for sell orders, high -> low for buy orders).

type SellOrderView = OrderView 'SellOrder

type BuyOrderView = OrderView 'BuyOrder

type SellOrders = AnnSet Summary SellOrderView

type BuyOrders = AnnSet Summary BuyOrderView

type Orders a = AnnSet Summary (OrderView a)

mkOrderView :: forall t. Known (OrderTypeToBias t) => OrderInfo t -> OrderView t
mkOrderView x =
  OrderView
    { orderInfo = x
    , bias = sing @(OrderTypeToBias t)
    }

-- | O (n (log n)). I think that's the best we can do while keeping this *reasonably* ergonomic and pure

-- NOTE: dunno if this more-polymorphic version will impact performance, do not delete the specialized versions
mkOrders ::
  forall t.
  (Known (OrderTypeToBias t), Ord (OrderView t)) =>
  [OrderInfo t] ->
  Orders t
mkOrders = foldl' go empty
 where
  go :: AnnSet Summary (OrderView t) -> OrderInfo t -> AnnSet Summary (OrderView t)
  go !acc !x = insert (mkOrderView x) acc

{-
{-# INLINE mkSellOrders #-}
mkSellOrders :: forall a. (Eq a) => (a -> Price) -> (a -> Volume) -> [a] -> SellOrders a
mkSellOrders !toPrice !toVolume =  snd . foldl' go (Priority 0,empty)
  where
    {-# INLINE go #-}
    go :: (Priority,AnnSet Volume (OrderView 'Min a)) -> a -> (Priority, AnnSet Volume (OrderView 'Min a))
    go (p@(Priority i),!acc) !x = (Priority (succ i), insert (mkOrderView @'Min toPrice toVolume p x) acc)

mkBuyOrders :: forall a. (Eq a) => (a -> Price) -> (a -> Volume) -> [a] -> BuyOrders a
mkBuyOrders toPrice toVolume =  fromList . snd . foldl' go (Priority 0,[])
  where
    go :: (Priority,[OrderView 'Max a]) -> a -> (Priority,[OrderView 'Max a])
    go (p@(Priority i),acc) x = (Priority (succ i), mkOrderView @'Max toPrice toVolume p x : acc)
-}
