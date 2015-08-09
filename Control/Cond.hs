{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Cond
    ( CondT(..), Cond

    -- * Executing CondT
    , runCondT, runCond, applyCondT

    -- * Promotions
    , guardM, guard_, guardM_, apply, consider

    -- * Boolean logic
    , matches, if_, when_, unless_, or_, and_, not_

    -- * Basic conditionals
    , accept, ignore, norecurse, prune

    -- * Helper functions
    , recurse, test
    )
    where

import Control.Applicative
import Control.Arrow ((***), first)
import Control.Monad hiding (mapM_, sequence_)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Morph
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.State (StateT(..), withStateT, evalStateT)
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (isJust)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Prelude hiding (mapM_, foldr1, sequence_)

data Recursor a m b = Stop
                    | Recurse (CondT a m b)
                    | Continue
    deriving Functor

instance Semigroup (Recursor a m b) where
    Stop      <> _         = Stop
    _         <> Stop      = Stop
    Recurse n <> _         = Recurse n
    _         <> Recurse n = Recurse n
    _         <> _         = Continue

instance Monoid (Recursor a m b) where
    mempty  = Continue
    mappend = (<>)

instance MFunctor (Recursor a) where
    hoist _   Stop        = Stop
    hoist nat (Recurse n) = Recurse (hoist nat n)
    hoist _   Continue    = Continue

type CondR a m b = (Maybe b, Recursor a m b)

accept' :: Monad m => b -> CondR a m b
accept' x = (Just x, Continue)
{-# INLINE accept' #-}

recurse' :: Monad m => CondR a m b
recurse' = (Nothing, Continue)
{-# INLINE recurse' #-}

-- | 'CondT' and its related combinators form a DSL to express whether, given
-- an item of type 'a': that item passes the predicate, and/or if recursion
-- should be performed from that item, should it relate to the branch of a
-- tree. This is used to build predicates that can guide recursive traversals.
--
-- For example, when recursing files in a directory tree, there are several
-- scenarios that 'CondT' maybe consider:
--
--   - Whether the entry at a given path is of interest, independent from its
--     type (files or directories)
--   - If the path is a directory, if the directory should be recursed into.
--
-- Yes or no answers are accepted for either criterion. This means that the
-- answer is "no" to both questions for a given directory, the combinator
-- 'prune' should be used both to ignore the entry itself, and to prevent
-- recursion into its contents.
--
-- Several different predicate types may be promoted to 'CondT':
--
--   [@Bool@]                  Using 'guard'
--
--   [@m Bool@]                Using 'guardM'
--
--   [@a -> Bool@]              Using 'guard_'
--
--   [@a -> m Bool@]            Using 'guardM_'
--
--   [@a -> m (Maybe b)@]       Using 'apply'
--
--   [@a -> m (Maybe (b, a))@]  Using 'consider'
--
-- Here is a trivial example:
--
-- @
-- flip runCondT 42 $ do
--   guard_ even
--   liftIO $ putStrLn "42 must be even to reach here"
--   guard_ odd \<|\> guard_ even
--   guard_ (== 42)
-- @
--
-- If 'CondT' is executed using 'runCondT', it returns a @Maybe b@ if the
-- predicate matched. It should usually be run with 'applyCondT', which calls
-- a continuation indicating wether recursion should be performed.
newtype CondT a m b = CondT { getCondT :: StateT a m (CondR a m b) }
    deriving Functor

type Cond a = CondT a Identity

instance (Monad m, Semigroup b) => Semigroup (CondT a m b) where
    (<>) = liftM2 (<>)
    {-# INLINE (<>) #-}

instance (Monad m, Monoid b) => Monoid (CondT a m b) where
    mempty  = CondT $ return mempty
    {-# INLINE mempty #-}
    mappend = liftM2 mappend
    {-# INLINE mappend #-}

instance (Monad m, Functor m) => Applicative (CondT a m) where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad m => Monad (CondT a m) where
    return = CondT . return . accept'
    {-# INLINE return #-}
    fail _ = mzero
    {-# INLINE fail #-}
    CondT m >>= k = CondT $ m >>= \case
        (Nothing, Stop)      -> return (Nothing, Stop)
        (Nothing, Continue)  -> return (Nothing, Continue)
        (Nothing, Recurse n) -> return (Nothing, Recurse (n >>= k))
        (Just b,  Stop)      -> fmap (const Stop) `liftM` getCondT (k b)
        (Just b,  Continue)  -> getCondT (k b)
        (Just b,  Recurse n) -> getCondT (k b) >>= \case
            (v, Continue) -> return (v, Recurse (n >>= k))
            x@_ -> return x
    {-# INLINEABLE (>>=) #-}

instance Monad m => MonadReader a (CondT a m) where
    ask               = CondT $ gets accept'
    {-# INLINE ask #-}
    local f (CondT m) = CondT $ withStateT f m
    {-# INLINE local #-}
    reader f          = liftM f ask
    {-# INLINE reader #-}

instance Monad m => MonadState a (CondT a m) where
    get     = CondT $ gets accept'
    {-# INLINE get #-}
    put s   = CondT $ liftM accept' $ put s
    {-# INLINE put #-}
    state f = CondT $ state (fmap (first accept') f)
    {-# INLINE state #-}

instance (Monad m, Functor m) => Alternative (CondT a m) where
    empty = CondT $ return recurse'
    {-# INLINE empty #-}
    CondT f <|> CondT g = CondT $ do
        r <- f
        case r of
            x@(Just _, _) -> return x
            _ -> g
    {-# INLINEABLE (<|>) #-}

instance Monad m => MonadPlus (CondT a m) where
    mzero = CondT $ return recurse'
    {-# INLINE mzero #-}
    mplus (CondT f) (CondT g) = CondT $ do
        r <- f
        case r of
            x@(Just _, _) -> return x
            _ -> g
    {-# INLINEABLE mplus #-}

instance MonadThrow m => MonadThrow (CondT a m) where
    throwM = CondT . throwM
    {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (CondT a m) where
    catch (CondT m) c = CondT $ m `catch` \e -> getCondT (c e)
    {-# INLINE catch #-}
#if MIN_VERSION_exceptions(0,6,0)

instance MonadMask m => MonadMask (CondT a m) where
#endif
    mask a = CondT $ mask $ \u -> getCondT (a $ q u)
      where q u = CondT . u . getCondT
    {-# INLINE mask #-}
    uninterruptibleMask a =
        CondT $ uninterruptibleMask $ \u -> getCondT (a $ q u)
      where q u = CondT . u . getCondT
    {-# INLINEABLE uninterruptibleMask #-}

instance MonadBase b m => MonadBase b (CondT a m) where
    liftBase m = CondT $ liftM accept' $ liftBase m
    {-# INLINE liftBase #-}

instance MonadIO m => MonadIO (CondT a m) where
    liftIO m = CondT $ liftM accept' $ liftIO m
    {-# INLINE liftIO #-}

instance MonadTrans (CondT a) where
    lift m = CondT $ liftM accept' $ lift m
    {-# INLINE lift #-}

#if MIN_VERSION_monad_control(1,0,0)
instance MonadBaseControl b m => MonadBaseControl b (CondT r m) where
    type StM (CondT r m) a = StM m (CondR r m a, r)
    liftBaseWith f = CondT $ StateT $ \s ->
        liftM (\x -> (accept' x, s)) $ liftBaseWith $ \runInBase ->
            f $ \k -> runInBase $ runStateT (getCondT k) s
    {-# INLINABLE liftBaseWith #-}
    restoreM = CondT . StateT . const . restoreM
    {-# INLINE restoreM #-}
#else
instance MonadBaseControl b m => MonadBaseControl b (CondT r m) where
    newtype StM (CondT r m) a =
        CondTStM { unCondTStM :: StM m (Result r m a, r) }
    liftBaseWith f = CondT $ StateT $ \s ->
        liftM (\x -> (accept' x, s)) $ liftBaseWith $ \runInBase -> f $ \k ->
            liftM CondTStM $ runInBase $ runStateT (getCondT k) s
    {-# INLINEABLE liftBaseWith #-}
    restoreM = CondT . StateT . const . restoreM . unCondTStM
    {-# INLINE restoreM #-}
#endif

instance MFunctor (CondT a) where
    hoist nat (CondT m) = CondT $ hoist nat (fmap (hoist nat) `liftM` m)
    {-# INLINE hoist #-}

runCondT :: Monad m => CondT a m b -> a -> m (Maybe b)
runCondT (CondT f) a = fst `liftM` evalStateT f a
{-# INLINE runCondT #-}

runCond :: Cond a b -> a -> Maybe b
runCond = (runIdentity .) . runCondT
{-# INLINE runCond #-}

-- | Case analysis by applying a condition to an input value.
applyCondT :: Monad m
           => a -> CondT a m b -> m (Maybe a, Maybe (CondT a m b))
applyCondT a c@(CondT (StateT s)) = go `liftM` s a
  where
    go (p, a') = (fmap (const a') *** recursorToMaybe c) p

    recursorToMaybe _ Stop        = Nothing
    recursorToMaybe p Continue    = Just p
    recursorToMaybe _ (Recurse n) = Just n

{-# INLINE applyCondT #-}

guardM :: MonadPlus m => m Bool -> m ()
guardM = (>>= guard)
{-# INLINE guardM #-}

guard_ :: (MonadPlus m, MonadReader a m) => (a -> Bool) -> m ()
guard_ f = ask >>= guard . f
{-# INLINE guard_ #-}

guardM_ :: (MonadPlus m, MonadReader a m) => (a -> m Bool) -> m ()
guardM_ f = ask >>= guardM . f
{-# INLINE guardM_ #-}

-- | Apply a value-returning predicate. Note that whether or not this return a
-- 'Just' value, recursion will be performed in the entry itself, if
-- applicable.
apply :: (MonadPlus m, MonadReader a m) => (a -> m (Maybe b)) -> m b
apply = asks >=> (>>= maybe mzero return)
{-# INLINE apply #-}

-- | Consider an element, as 'apply', but returning a mutated form of the
-- element. This can be used to apply optimizations to speed future
-- conditions.
consider :: (MonadPlus m, Functor m, MonadState a m)
         => (a -> m (Maybe (b, a))) -> m b
consider = gets >=> (>>= maybe mzero (\(b, a') -> b <$ put a'))
{-# INLINE consider #-}

accept :: MonadPlus m => m ()
accept = return ()
{-# INLINE accept #-}

-- | 'ignore' ignores the current entry, but allows recursion into its
--   descendents.  This is the same as 'empty'.
ignore :: MonadPlus m => m b
ignore = mzero
{-# INLINE ignore #-}

-- | 'norecurse' prevents recursion into the current entry's descendents, but
--   does not ignore the entry itself.
norecurse :: Monad m => CondT a m ()
norecurse = CondT $ return (Just (), Stop)
{-# INLINE norecurse #-}

-- | 'prune' is a synonym for both ignoring an entry and its descendents. It
--   is the same as @ignore >> norecurse@.
prune :: Monad m => CondT a m b
prune = CondT $ return (Nothing, Stop)
{-# INLINE prune #-}

-- | Return True or False depending on whether the given condition matches or
--   not.  This differs from simply stating the condition in that it itself
--   always succeeds.
--
-- >>> flip runCond "foo.hs" $ matches (guard =<< asks (== "foo.hs"))
-- Just True
-- >>> flip runCond "foo.hs" $ matches (guard =<< asks (== "foo.hi"))
-- Just False
matches :: (Monad m, Functor m) => CondT a m b -> CondT a m Bool
matches = liftM isJust . optional
{-# INLINE matches #-}

-- | A variant of ifM which branches on whether the condition succeeds or not.
--   Note that @if_ x@ is equivalent to @ifM (matches x)@, and is provided
--   solely for convenience.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ if_ good (return "Success") (return "Failure")
-- Just "Success"
-- >>> flip runCond "foo.hs" $ if_ bad (return "Success") (return "Failure")
-- Just "Failure"
if_ :: Monad m => CondT a m r -> CondT a m b -> CondT a m b -> CondT a m b
if_ c x y = CondT $ do
    t <- getCondT c
    getCondT $ maybe y (const x) (fst t)
{-# INLINE if_ #-}

-- | 'when_' is just like 'when', except that it executes the body if the
--   condition passes, rather than based on a Bool value.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ when_ good ignore
-- Nothing
-- >>> flip runCond "foo.hs" $ when_ bad ignore
-- Just ()
when_ :: Monad m => CondT a m r -> CondT a m () -> CondT a m ()
when_ c x = if_ c x (return ())
{-# INLINE when_ #-}

-- | 'when_' is just like 'when', except that it executes the body if the
--   condition fails, rather than based on a Bool value.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ unless_ bad ignore
-- Nothing
-- >>> flip runCond "foo.hs" $ unless_ good ignore
-- Just ()
unless_ :: Monad m => CondT a m r -> CondT a m () -> CondT a m ()
unless_ c = if_ c (return ())
{-# INLINE unless_ #-}

-- | Check whether at least one of the given conditions is true.  This is a
--   synonym for 'Data.Foldable.asum'.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ or_ [bad, good]
-- Just ()
-- >>> flip runCond "foo.hs" $ or_ [bad]
-- Nothing
or_ :: Monad m => [CondT a m b] -> CondT a m b
or_ = Data.Foldable.msum
{-# INLINE or_ #-}

-- | Check that all of the given conditions are true.  This is a synonym for
--   'Data.Foldable.sequence_'.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ and_ [bad, good]
-- Nothing
-- >>> flip runCond "foo.hs" $ and_ [good]
-- Just ()
and_ :: Monad m => [CondT a m b] -> CondT a m ()
and_ = sequence_
{-# INLINE and_ #-}

-- | 'not_' inverts the meaning of the given predicate.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ not_ bad >> return "Success"
-- Just "Success"
-- >>> flip runCond "foo.hs" $ not_ good >> return "Shouldn't reach here"
-- Nothing
not_ :: Monad m => CondT a m b -> CondT a m ()
not_ c = when_ c ignore
{-# INLINE not_ #-}

-- | 'recurse' changes the recursion predicate for any child elements.  For
--   example, the following file-finding predicate looks for all @*.hs@ files,
--   but under any @.git@ directory looks only for a file named @config@:
--
-- @
-- if_ (name_ \".git\" \>\> directory)
--     (ignore \>\> recurse (name_ \"config\"))
--     (glob \"*.hs\")
-- @
--
-- NOTE: If this code had used @recurse (glob \"*.hs\"))@ instead in the else
-- case, it would have meant that @.git@ is only looked for at the top-level
-- of the search (i.e., the top-most element).
recurse :: Monad m => CondT a m b -> CondT a m b
recurse c = CondT $ fmap (const (Recurse c)) `liftM` getCondT c
{-# INLINE recurse #-}

-- | A specialized variant of 'runCondT' that simply returns True or False.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runIdentity $ test "foo.hs" $ not_ bad >> return "Success"
-- True
-- >>> runIdentity $ test "foo.hs" $ not_ good >> return "Shouldn't reach here"
-- False
test :: Monad m => a -> CondT a m b -> m Bool
test = (liftM isJust .) . flip runCondT
{-# INLINE test #-}
