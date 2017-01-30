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
import qualified Pipes.Misc as PM

-- | Converts a 'Glazier.Gadget' into a 'Pipes.Pipe'
gadgetToPipe :: (Monad m, MonadTrans t, MonadState s (t m)) => G.Gadget s m a c -> P.Pipe a c (t m) r
gadgetToPipe g = forever $ do
    a <- P.await
    s <- get
    -- This is the only line that is different between the Strict and Lazy version
    (c, s') <- lift . lift $ view G._Gadget g a s
    put s'
    P.yield c

-- | Convert a 'Pipes.Concurrent.Input' and a 'Glazier.Gadget' into a stateful 'Pipes.Producer' of commands to interpret.
gadgetToProducer ::
  (MonadState s (t STM), MonadTrans t) =>
  PC.Input a -> G.Gadget s STM a c -> P.Producer' c (t STM) ()
gadgetToProducer input g = hoist lift (PM.fromInputSTM input) P.>-> gadgetToPipe g


-- | This is similar to part of the Elm startApp.
-- This is responsible for running the glazier widget update tick until it quits.
-- This is also responsible for rendering the frame and interpreting commands.
runUi :: (MonadIO io) =>
     Int
  -> s
  -> (cmd -> MaybeT IO ()) -- interpretCmds
  -> (s -> MaybeT IO ()) -- render
  -> P.Producer cmd (StateT s STM) ()
  -> io s
runUi refreshDelay initialState interpretCmds render appSig = do
    -- framerate thread
    -- TMVar to indicate that the render thread can render, start non empty so we can render straight away.
    triggerRender <- liftIO $ newTMVarIO ()
    frameRateThread <-
        liftIO $
        forkIO . void . forever $
        -- The will ensure a refreshDelay in between times when value in canRender is taken.
        -- wait until canRender is empty (ie taken by the render thread)
         do
            atomically $ STE.waitTillEmptyTMVar triggerRender ()
            -- if empty, then wait delay before filling TMVar with next canRender value
            threadDelay refreshDelay
            atomically $ putTMVar triggerRender ()

    -- render thread
    enableRenderThread <- liftIO $ newTMVarIO ()
    finishedRenderThread <- liftIO newEmptyTMVarIO
    latestState <- liftIO $ newTMVarIO initialState
    void . liftIO $
        forkFinally
            (void . runMaybeT . forever $
              -- check if we can start render
              do
                 liftIO . atomically . void $ takeTMVar triggerRender
                 -- to allow rendering of last frame before quitting
                 -- if there is no state to render, check if rendering is disabled
                 s <-
                     MaybeT . liftIO . atomically $
                     (Just <$> takeTMVar latestState) `orElse` do
                         r <- tryReadTMVar enableRenderThread
                         case r of
                             Nothing -> pure Nothing -- breaks (runMaybeT . forever) loop
                             Just _ -> retry
                 render s)
            (const . atomically $ putTMVar finishedRenderThread ())
    -- hoist to atomically apply the STM
    -- then lift from StateT s IO -> MaybeT (StateT s IO)
    s' <-
        liftIO $
        P.runEffect $
        PL.execStateP initialState $
        PL.runMaybeP $
        -- hoist MaybeT STM -> MaybeT (StateT STM), then lift into the Pipe
        P.for (hoist
                  (lift . hoist (liftIO . atomically))
                  (appSig P.>-> PM.onState (void . lift . STE.forceSwapTMVar latestState))) $ \c ->
            lift $ hoist lift (interpretCmds c)

    -- cleanup
    -- allow rendering of the frame one last time
    liftIO . atomically $ takeTMVar enableRenderThread
    -- wait for render thread to finish before exiting
    liftIO . atomically $ takeTMVar finishedRenderThread
    -- kill frameRateThread only after render thread has finished
    -- since renderThread waits on triggers from frameRateThread
    liftIO $ killThread frameRateThread
    -- liftIO $ killThread ctlsThread
    -- return final state
    liftIO $ pure s'
