{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Control.Cond
    ( CondT, Cond

    -- * Executing CondT
    , runCondT, runCond, execCondT, evalCondT, test

    -- * Promotions
    , MonadQuery(..), guardM, guard_, guardM_, apply, consider

    -- * Basic conditionals
    , accept, ignore, norecurse, prune

    -- * Boolean logic
    , matches, ifM, whenM, unlessM
    , if_, when_, unless_, or_, and_, not_

    -- * helper functions
    , recurse
    )
    where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad hiding (mapM_, sequence_)
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Cont.Class as C
import           Control.Monad.Error.Class as E
import           Control.Monad.Fix
import           Control.Monad.Morph as M
import           Control.Monad.Reader.Class as R
import           Control.Monad.State.Class as S
import           Control.Monad.Trans
import           Control.Monad.Trans.Cont (ContT(..))
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Error (ErrorT(..))
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Trans.Identity (IdentityT(..))
import           Control.Monad.Trans.List (ListT(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Control.Monad.Writer.Class
import           Control.Monad.Zip
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Monoid hiding ((<>))
import           Data.Semigroup
import           Prelude hiding (mapM_, foldr1, sequence_)

data Recursor a m r = Stop | Recurse (CondT a m r) | Continue
    deriving Functor

instance Semigroup (Recursor a m r) where
    Stop      <> _         = Stop
    _         <> Stop      = Stop
    Recurse n <> _         = Recurse n
    _         <> Recurse n = Recurse n
    _         <> _         = Continue
    {-# INLINE (<>) #-}

instance Monoid (Recursor a m r) where
    mempty  = Continue
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance MFunctor (Recursor a) where
    hoist _   Stop        = Stop
    hoist nat (Recurse n) = Recurse (hoist nat n)
    hoist _   Continue    = Continue
    {-# INLINE hoist #-}

type CondR a m r = (Maybe r, Recursor a m r)

accept' :: Monad m => r -> CondR a m r
accept' x = (Just x, Continue)
{-# INLINE accept' #-}

recurse' :: Monad m => CondR a m r
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
--   [@a -> m (Maybe r)@]       Using 'apply'
--
--   [@a -> m (Maybe (r, a))@]  Using 'consider'
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
-- If 'CondT' is executed using 'runCondT', it returns a @Maybe r@ if the
-- predicate matched. It should usually be run with 'applyCondT', which calls
-- a continuation indicating wether recursion should be performed.
newtype CondT a m r = CondT { getCondT :: StateT a m (CondR a m r) }
    deriving Functor

type Cond a = CondT a Identity

instance (Monad m, Semigroup r) => Semigroup (CondT a m r) where
    (<>) = liftM2 (<>)
    {-# INLINE (<>) #-}

instance (Monad m, Monoid r) => Monoid (CondT a m r) where
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
        (Just r,  Stop)      -> fmap (const Stop) `liftM` getCondT (k r)
        (Just r,  Continue)  -> getCondT (k r)
        (Just r,  Recurse n) -> getCondT (k r) >>= \case
            (v, Continue) -> return (v, Recurse (n >>= k))
            x             -> return x
    {-# INLINEABLE (>>=) #-}
#if __GLASGOW_HASKELL__ >= 710
    {-# SPECIALIZE (>>=)
          :: CondT e IO a -> (a -> CondT e IO b) -> CondT e IO b #-}
#endif

instance MonadReader r m => MonadReader r (CondT a m) where
    ask = lift R.ask
    {-# INLINE ask #-}
    local f (CondT m) = CondT $ R.local f m
    {-# INLINE local #-}
    reader = lift . R.reader
    {-# INLINE reader #-}

instance MonadWriter w m => MonadWriter w (CondT a m) where
    writer   = lift . writer
    {-# INLINE writer #-}
    tell     = lift . tell
    {-# INLINE tell #-}
    listen m = m >>= lift . listen . return
    {-# INLINE listen #-}
    pass m   = m >>= lift . pass . return
    {-# INLINE pass #-}

instance MonadState s m => MonadState s (CondT a m) where
    get = lift S.get
    {-# INLINE get #-}
    put = lift . S.put
    {-# INLINE put #-}
    state = lift . S.state
    {-# INLINE state #-}

instance (Monad m, Functor m) => Alternative (CondT a m) where
    empty = CondT $ return recurse'
    {-# INLINE empty #-}
    CondT f <|> CondT g = CondT $ do
        r <- f
        case r of
            x@(Just _, _) -> return x
            _ -> g
    {-# INLINE (<|>) #-}

instance Monad m => MonadPlus (CondT a m) where
    mzero = CondT $ return recurse'
    {-# INLINE mzero #-}
    mplus (CondT f) (CondT g) = CondT $ do
        r <- f
        case r of
            x@(Just _, _) -> return x
            _ -> g
    {-# INLINE mplus #-}

instance MonadError e m => MonadError e (CondT a m) where
    throwError = CondT . throwError
    {-# INLINE throwError #-}
    catchError (CondT m) h = CondT $ m `catchError` \e -> getCondT (h e)
    {-# INLINE catchError #-}

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

-- This won't work for StateT-like types
-- instance MMonad (CondT a) where
--     embed f m = undefined
--     {-# INLINE embed #-}

instance MonadCont m => MonadCont (CondT a m) where
    callCC f = CondT $ StateT $ \a ->
        callCC $ \k -> flip runStateT a $ getCondT $ f $ \r ->
            CondT $ StateT $ \a' -> k ((Just r, Continue), a')

instance Monad m => MonadZip (CondT a m) where
    mzipWith = liftM2
    {-# INLINE mzipWith #-}

-- A deficiency of this instance is that recursion uses the same initial 'a'.
instance MonadFix m => MonadFix (CondT a m) where
    mfix f = CondT $ StateT $ \a -> mdo
        ((mb, n), a') <- case mb of
            Nothing -> return ((mb, n), a')
            Just b  -> runStateT (getCondT (f b)) a
        return ((mb, n), a')

-- | Apply a condition to an input value, returning a (possibly) updated copy
-- of that value if it matches, and the next 'CondT' to use if recursion into
-- that value was indicated.
runCondT :: Monad m => a -> CondT a m r -> m ((Maybe r, Maybe (CondT a m r)), a)
runCondT a c@(CondT (StateT s)) = go `liftM` s a
  where
    {-# INLINE go #-}
    go (p, a') = (second (recursorToMaybe c) p, a')

    {-# INLINE recursorToMaybe #-}
    recursorToMaybe _ Stop        = Nothing
    recursorToMaybe p Continue    = Just p
    recursorToMaybe _ (Recurse n) = Just n
{-# INLINE runCondT #-}

runCond :: a -> Cond a r -> Maybe r
runCond = ((fst . fst . runIdentity) .) . runCondT
{-# INLINE runCond #-}

execCondT :: Monad m => a -> CondT a m r -> m (Maybe a, Maybe (CondT a m r))
execCondT a c = go `liftM` runCondT a c
  where
    go ((mr, mnext), a') = (const a' <$> mr, mnext)
{-# INLINE execCondT #-}

evalCondT :: Monad m => a -> CondT a m r -> m (Maybe r)
evalCondT a c = go `liftM` runCondT a c
  where
    go ((mr, _), _) = mr
{-# INLINE evalCondT #-}

-- | A specialized variant of 'runCondT' that simply returns True or False.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runIdentity $ test "foo.hs" $ not_ bad >> return "Success"
-- True
-- >>> runIdentity $ test "foo.hs" $ not_ good >> return "Shouldn't reach here"
-- False
test :: Monad m => a -> CondT a m r -> m Bool
test a c = go `liftM` runCondT a c
  where
    go ((Nothing, _), _) = False
    go ((Just _, _), _)  = True
{-# INLINE test #-}

-- | 'MonadQuery' is a custom version of 'MonadReader', created so that users
-- could still have their own 'MonadReader' accessible within conditionals.
class Monad m => MonadQuery a m | m -> a where
    query :: m a
    queries :: (a -> b) -> m b
    update :: a -> m ()
    updates :: (a -> a) -> m ()

instance Monad m => MonadQuery a (CondT a m) where
    -- | Returns the item currently under consideration.
    query = CondT $ gets accept'
    {-# INLINE query #-}

    -- | Returns the item currently under consideration while applying a
    -- function, in the spirit of 'asks'.
    queries f = CondT $ state (\a -> (accept' (f a), a))
    {-# INLINE queries #-}

    update a = CondT $ liftM accept' $ put a
    {-# INLINE update #-}

    updates f = CondT $ liftM accept' $ modify f
    {-# INLINE updates #-}

instance MonadQuery r m => MonadQuery r (ReaderT r m) where
    query = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance (MonadQuery r m, Monoid w) => MonadQuery r (LazyRWS.RWST r w s m) where
    query = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance (MonadQuery r m, Monoid w)
         => MonadQuery r (StrictRWS.RWST r w s m) where
    query = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

-- All of these instances need UndecidableInstances, because they do not satisfy
-- the coverage condition.

instance MonadQuery r' m => MonadQuery r' (ContT r m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance (Error e, MonadQuery r m) => MonadQuery r (ErrorT e m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance MonadQuery r m => MonadQuery r (ExceptT e m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance MonadQuery r m => MonadQuery r (IdentityT m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance MonadQuery r m => MonadQuery r (ListT m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance MonadQuery r m => MonadQuery r (MaybeT m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance MonadQuery r m => MonadQuery r (Lazy.StateT s m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance MonadQuery r m => MonadQuery r (Strict.StateT s m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance (Monoid w, MonadQuery r m) => MonadQuery r (Lazy.WriterT w m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

instance (Monoid w, MonadQuery r m) => MonadQuery r (Strict.WriterT w m) where
    query   = lift query
    {-# INLINE query #-}
    queries = lift . queries
    {-# INLINE queries #-}
    update = lift . update
    {-# INLINE update #-}
    updates = lift . updates
    {-# INLINE updates #-}

guardM :: MonadPlus m => m Bool -> m ()
guardM = (>>= guard)
{-# INLINE guardM #-}

guard_ :: (MonadPlus m, MonadQuery a m) => (a -> Bool) -> m ()
guard_ f = query >>= guard . f
{-# INLINE guard_ #-}

guardM_ :: (MonadPlus m, MonadQuery a m) => (a -> m Bool) -> m ()
guardM_ f = query >>= guardM . f
{-# INLINE guardM_ #-}

-- | Apply a value-returning predicate. Note that whether or not this return a
-- 'Just' value, recursion will be performed in the entry itself, if
-- applicable.
apply :: (MonadPlus m, MonadQuery a m) => (a -> m (Maybe r)) -> m r
apply = queries >=> (>>= maybe mzero return)
{-# INLINE apply #-}

-- | Consider an element, as 'apply', but returning a mutated form of the
-- element. This can be used to apply optimizations to speed future
-- conditions.
consider :: (MonadPlus m, MonadQuery a m) => (a -> m (Maybe (r, a))) -> m r
consider = queries >=> (>>= maybe mzero (\(r, a') -> const r `liftM` update a'))
{-# INLINE consider #-}

accept :: MonadPlus m => m ()
accept = return ()
{-# INLINE accept #-}

-- | 'ignore' ignores the current entry, but allows recursion into its
--   descendents.  This is the same as 'empty'.
ignore :: MonadPlus m => m r
ignore = mzero
{-# INLINE ignore #-}

-- | 'norecurse' prevents recursion into the current entry's descendents, but
--   does not ignore the entry itself.
norecurse :: Monad m => CondT a m ()
norecurse = CondT $ return (Just (), Stop)
{-# INLINE norecurse #-}

-- | 'prune' is a synonym for both ignoring an entry and its descendents.
prune :: Monad m => CondT a m r
prune = CondT $ return (Nothing, Stop)
{-# INLINE prune #-}

-- | Return True or False depending on whether the given condition matches or
--   not.  This differs from simply stating the condition in that it itself
--   always succeeds.
--
-- >>> runCond "foo.hs" $ matches (guard =<< queries (== "foo.hs"))
-- Just True
-- >>> runCond "foo.hs" $ matches (guard =<< queries (== "foo.hi"))
-- Just False
matches :: MonadPlus m => m r -> m Bool
matches m = (const True `liftM` m) `mplus` return False
{-# INLINE matches #-}

ifM :: Monad m => m Bool -> m s -> m s -> m s
ifM c x y = c >>= \b -> if b then x else y
{-# INLINE ifM #-}

-- | A variant of ifM which branches on whether the condition succeeds or not.
--   Note that @if_ x@ is equivalent to @ifM (matches x)@, and is provided
--   solely for convenience.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runCond "foo.hs" $ if_ good (return "Success") (return "Failure")
-- Just "Success"
-- >>> runCond "foo.hs" $ if_ bad (return "Success") (return "Failure")
-- Just "Failure"
if_ :: MonadPlus m => m r -> m s -> m s -> m s
if_ c x y = matches c >>= \b -> if b then x else y
{-# INLINE if_ #-}

whenM :: Monad m => m Bool -> m s -> m ()
whenM c x = ifM c (x >> return ()) (return ())
{-# INLINE whenM #-}

-- | 'when_' is just like 'when', except that it executes the body if the
--   condition passes, rather than based on a Bool value.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runCond "foo.hs" $ when_ good ignore
-- Nothing
-- >>> runCond "foo.hs" $ when_ bad ignore
-- Just ()
when_ :: MonadPlus m => m r -> m s -> m ()
when_ c x = if_ c (x >> return ()) (return ())
{-# INLINE when_ #-}

unlessM :: Monad m => m Bool -> m s -> m ()
unlessM c x = ifM c (return ()) (x >> return ())
{-# INLINE unlessM #-}

-- | 'when_' is just like 'when', except that it executes the body if the
--   condition fails, rather than based on a Bool value.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runCond "foo.hs" $ unless_ bad ignore
-- Nothing
-- >>> runCond "foo.hs" $ unless_ good ignore
-- Just ()
unless_ :: MonadPlus m => m r -> m s -> m ()
unless_ c x = if_ c (return ()) (x >> return ())
{-# INLINE unless_ #-}

-- | Check whether at least one of the given conditions is true.  This is a
--   synonym for 'Data.Foldable.asum'.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runCond "foo.hs" $ or_ [bad, good]
-- Just ()
-- >>> runCond "foo.hs" $ or_ [bad]
-- Nothing
or_ :: MonadPlus m => [m r] -> m r
or_ = Data.Foldable.msum
{-# INLINE or_ #-}

-- | Check that all of the given conditions are true.  This is a synonym for
--   'Data.Foldable.sequence_'.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runCond "foo.hs" $ and_ [bad, good]
-- Nothing
-- >>> runCond "foo.hs" $ and_ [good]
-- Just ()
and_ :: MonadPlus m => [m r] -> m ()
and_ = sequence_
{-# INLINE and_ #-}

-- | 'not_' inverts the meaning of the given predicate.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runCond "foo.hs" $ not_ bad >> return "Success"
-- Just "Success"
-- >>> runCond "foo.hs" $ not_ good >> return "Shouldn't reach here"
-- Nothing
not_ :: MonadPlus m => m r -> m ()
not_ c = if_ c ignore accept
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
recurse :: Monad m => CondT a m r -> CondT a m r
recurse c = CondT $ fmap (const (Recurse c)) `liftM` getCondT c
{-# INLINE recurse #-}
