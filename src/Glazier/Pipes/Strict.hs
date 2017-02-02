{-# LANGUAGE RankNTypes #-}

module Glazier.Pipes.Strict where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar.Extras as STE
import Control.Lens
import Control.Monad.Morph
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Glazier.Strict as G
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Misc.Concurrent as PM
import qualified Pipes.Misc.State.Strict as PM
import qualified Pipes.Prelude as PP

import qualified Pipes.Internal as PI


-- | Converts a 'Glazier.Gadget' into a 'Pipes.Pipe'
gadgetToPipe :: (Monad m, MonadTrans t, MonadState s (t m)) => G.Gadget s m a c -> P.Pipe a c (t m) r
gadgetToPipe g = forever $ do
    a <- P.await
    s <- get
    -- This is the only line that is different between the Strict and Lazy version
    (c, s') <- lift . lift $ view G._Gadget g a s
    put s'
    P.yield c
{-# INLINABLE gadgetToPipe #-}

-- | Convert a 'Pipes.Concurrent.Input' and a 'Glazier.Gadget' into a stateful 'Pipes.Producer' of commands to interpret.
gadgetToProducer ::
  (MonadState s (t STM), MonadTrans t) =>
  PC.Input a -> G.Gadget s STM a c -> P.Producer' c (t STM) ()
gadgetToProducer input g = hoist lift (PM.fromInputSTM input) P.>-> gadgetToPipe g
{-# INLINABLE gadgetToProducer #-}
