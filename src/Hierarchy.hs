{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hierarchy where

import Control.Monad
import Control.Comonad.Trans.Cofree
import Control.Cond

-- | A 'TreeT' is a tree of values, where the (possible) branches are
--   represented by some MonadPlus 'm'.
type TreeT m = CofreeT Maybe m

-- | Turn a list into a series of possibilities:
select :: MonadPlus m => [a] -> m a
select = msum . map pure

-- | Descend one level into a 'TreeT', yielding a list of values and their
--   possible associated trees.
descend :: MonadPlus m => TreeT m a -> m (a, Maybe (TreeT m a))
descend (CofreeT t) = t >>= \(a :< mp) -> pure (a, mp)
{-# INLINE descend #-}

-- | Perform a depth-first traversal of a 'TreeT', yielding each of its
--   contents. Note that breadth-first traversals cannot offer static memory
--   guarantees, so they are not provided by this module.
walk :: MonadPlus m => TreeT m a -> m a
walk (CofreeT t) = t >>= \(a :< mp) -> pure a `mplus` maybe mzero walk mp
{-# INLINEABLE walk #-}

-- | Given a 'TreeT', produce another 'TreeT' which yields only those elements
--   (and sub-trees) matching the given monadic conditional. This conditional
--   (see 'Control.Cond.CondT') can choose both elements and points of
--   recursion, making it capable of expressing any tree traversal in the form
--   of a predicate DSL. This differs from an expression-based traversal, like
--   XPath or Lens, in that effects in 'm' may be used to guide selection.
--
--   For example, to print all Haskell files under the current directory:
--
-- @
-- let files = winnow (directoryFiles ".") $ do
--         path <- query
--         liftIO $ putStrLn $ "Considering " ++ path
--         when (path @`elem@` [".@/@.git", ".@/@dist", ".@/@result"])
--             prune  -- ignore these, and don't recurse into them
--         guard_ (".hs" @`isInfixOf@`)  -- implicitly references 'path'
-- forM_ (walk files) $ liftIO . print
-- @
winnow :: MonadPlus m => TreeT m a -> CondT a m () -> TreeT m a
winnow (CofreeT t) p = CofreeT $ t >>= \(a :< mst) -> do
    (mval, mnext) <- execCondT a p
    let mnext' = winnow <$> mst <*> mnext
    case mval of
        Nothing -> maybe mzero runCofreeT mnext'
        Just a' -> pure $ a' :< mnext'
