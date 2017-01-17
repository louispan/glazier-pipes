{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.Pipes.Lazy where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.RWS.Lazy
import qualified Glazier.Lazy as G
import qualified Pipes as P
import qualified Pipes.Misc as PM

-- | Converts a Gadget into a Pipe
gadgetToPipe :: (MonadWriter w m, MonadState s m) => G.Gadget a w s m c -> P.Pipe a c m r
gadgetToPipe (G.Gadget n) = forever $ do
    a <- P.await
    s <- get
    (c, s', w) <- lift $ runRWST n a s
    put s'
    tell w
    P.yield c

-- | Like Gadget Pipe, but ignores the Writer effects
gadgetToPipe' :: (MonadState s m) => G.Gadget a w s m c -> P.Pipe a c m r
gadgetToPipe' (G.Gadget n) = P.for P.cat $ \a -> do
    s <- get
    (c, s', _) <- lift $ runRWST n a s
    put s'
    P.yield c
