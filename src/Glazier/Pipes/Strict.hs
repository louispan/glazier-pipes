module Glazier.Pipes.Strict where

import Control.Monad.State.Strict
import qualified Glazier.Strict as G
import qualified Pipes as P
import Control.Lens

-- | Converts a Gadget into a Pipe
gadgetToPipe :: (Monad m, MonadTrans t, MonadState s (t m)) => G.Gadget s m a c -> P.Pipe a c (t m) r
gadgetToPipe g = forever $ do
    a <- P.await
    s <- get
    (c, s') <- lift . lift $ view G._GadgetT g a s
    put s'
    P.yield c
