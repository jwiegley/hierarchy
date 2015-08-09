{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipes.Tree where

import Control.Applicative
import Control.Comonad.Trans.Cofree
import Control.Cond
import Control.Exception
import Control.Monad
import Pipes
import System.Directory
import System.Posix.Files

-- | A 'TreeT' is a tree of values, where the (possible) branches are
-- 'ListT's.
type TreeT m = CofreeT Maybe (ListT m)

-- | Turn an old-style generated list into a 'ListT'.
selectEach :: Monad m => m [a] -> ListT m a
selectEach m = Select $ each =<< lift m

-- | Return all files within a directory tree, hierarchically.
directoryFiles :: MonadIO m => FilePath -> TreeT m FilePath
directoryFiles path = CofreeT $ Select $ do
    eres <- liftIO $ try $ getDirectoryContents path
    case eres of
        Left (_ :: IOException) -> return ()
            -- liftIO $ putStrLn $
            --     "Error reading directory " ++ path ++ ": " ++ show e
        Right entries -> forM_ entries $ \entry ->
            unless (entry `elem` [".", ".."]) $ do
                let entryPath = path ++ "/" ++ entry
                estat <- liftIO $ try $ getFileStatus entryPath
                case estat of
                    Left (_ :: IOException) -> return ()
                    Right stat ->
                        yield (entryPath :< if isDirectory stat
                                            then Just (directoryFiles entryPath)
                                            else Nothing)

-- | Descend one level into a 'TreeT', yielding a list of values and their
-- possible associated trees.
descend :: Monad m => TreeT m a -> ListT m (a, Maybe (TreeT m a))
descend (CofreeT (Select t)) = Select $ for t $ \(a :< mp) -> yield (a, mp)
{-# INLINE descend #-}

-- | Perform a depth-first traversal of a 'TreeT', yielding a 'ListT' of its
-- contents. Note that breadth-first traversals cannot offer static memory
-- guarantees, so they are not provided by this module.
walk :: Monad m => TreeT m a -> ListT m a
walk (CofreeT (Select t)) = Select $ for t $ \(a :< mp) ->
    yield a >> maybe (return ()) (enumerate . walk) mp
{-# INLINEABLE walk #-}

-- | Given a 'TreeT', produce another 'TreeT' which yields only those elements
-- (and sub-trees) matching the given monadic conditional. This conditional
-- (see 'Control.Cond.CondT') can choose both elements and points of
-- recursion, making it capable of expressing any tree traversal in the form
-- of a predicate DSL. This differs from an expression-based traversal, like
-- XPath or Lens, in that effects in 'm' may be used to guide selection.
--
-- For example, to print all Haskell files under the current directory:
--
-- @
--     let files = winnow (directoryFiles ".") $ do
--             path <- ask
--             liftIO $ putStrLn $ "Considering " ++ path
--             when (path @`elem@` [".@/@.git", ".@/@dist", ".@/@result"])
--                 prune  -- ignore these, and don't recurse into them
--             guard_ (".hs" @`isInfixOf@`)  -- implicitly references 'path'
--     runEffect $ for (enumerate (walk files)) $ liftIO . print
-- @
winnow :: Monad m => TreeT m a -> CondT a m () -> TreeT m a
winnow (CofreeT (Select t)) p = CofreeT $ Select $ for t $ \(a :< mst) -> do
    (mval, mnext) <- lift $ applyCondT a p
    let mnext' = winnow <$> mst <*> mnext
    case mval of
        Nothing -> maybe (return ()) (enumerate . runCofreeT) mnext'
        Just a' -> yield (a' :< mnext')
