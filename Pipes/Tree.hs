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

type TreeT m = CofreeT Maybe (ListT m)

selectM :: Monad m => m [a] -> ListT m a
selectM m = Select $ each =<< lift m

directoryFiles :: MonadIO m => FilePath -> TreeT m FilePath
directoryFiles path = CofreeT $ Select $ do
    eres <- liftIO $ try $ getDirectoryContents path
    case eres of
        Left (e :: IOException) ->
            liftIO $ putStrLn $
                "Error reading directory " ++ path ++ ": " ++ show e
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
{-# INLINEABLE descend #-}

-- | Perform a depth-first traversal of a 'TreeT', yielding a 'ListT' of its
-- contents. Note that breadth-first traversals cannot offer static memory
-- guarantees, so they are not provided by this module.
walk :: Monad m => TreeT m a -> ListT m a
walk (CofreeT (Select t)) = Select $ for t $ \(a :< mp) -> do
    yield a
    case mp of
        Nothing -> return ()
        Just p  -> enumerate (walk p)
{-# INLINEABLE walk #-}

winnow :: Monad m => TreeT m a -> CondT a m () -> TreeT m a
winnow (CofreeT (Select t)) p = CofreeT $ Select $ for t $ \(a :< mst) -> do
    (res, a') <- lift $ applyCondT a p
    case res of
        (Nothing, Nothing) -> return ()
        (Just (), Nothing) -> yield (a' :< Nothing)
        (Nothing, Just n)  -> case mst of
            Nothing -> return ()
            Just st -> enumerate $ runCofreeT $ winnow st n
        (Just (), Just n)  -> yield $ a' :< (flip winnow n <$> mst)
