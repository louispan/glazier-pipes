{-# LANGUAGE RankNTypes #-}

module Glazier.Pipes.Lazy where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar.Extras as STE
import Control.Lens
import Control.Monad.Morph
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import qualified Glazier.Lazy as G
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Misc.Concurrent as PM
import qualified Pipes.Misc.State.Lazy as PM

-- | Converts a 'Glazier.Gadget' into a 'Pipes.Pipe'
gadgetToPipe :: (Monad m, MonadTrans t, MonadState s (t m)) => G.Gadget s m a c -> P.Pipe a c (t m) r
gadgetToPipe g = forever $ do
    a <- P.await
    s <- get
    -- This is the only line that is different between the Strict and Lazy version
    ~(c, s') <- lift . lift $ view G._Gadget g a s
    put s'
    P.yield c

-- | Convert a 'Pipes.Concurrent.Input' and a 'Glazier.Gadget' into a stateful 'Pipes.Producer' of commands to interpret.
gadgetToProducer ::
  (MonadState s (t STM), MonadTrans t) =>
  PC.Input a -> G.Gadget s STM a c -> P.Producer' c (t STM) ()
gadgetToProducer input g = hoist lift (PM.fromInputSTM input) P.>-> gadgetToPipe g

-- | This is similar to part of the Elm startApp.
-- This is responsible for running the Glazier Gadget update tick until it quits.
-- This is also responsible for rendering the frame.
runUi :: (MonadIO io) =>
     Int
  -> (s -> IO ()) -- render
  -> s
  -> P.Effect (MaybeT (StateT s io)) ()
  -> io s
runUi refreshDelay render initialState appEffect = do
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
                 lift $ render s)
            (const . atomically $ putTMVar finishedRenderThread ())

    -- This is different between the Strict and Lazy version
    s' <- (`execStateT` initialState) $
        P.runEffect $
        PL.runMaybeP $
        appEffect P.>-> PM.onState
            (liftIO . atomically . void . STE.forceSwapTMVar latestState)
    -- cleanup
    -- allow rendering of the frame one last time
    liftIO . atomically $ takeTMVar enableRenderThread
    -- wait for render thread to finish before exiting
    liftIO . atomically $ takeTMVar finishedRenderThread
    -- kill frameRateThread only after render thread has finished
    -- since renderThread waits on triggers from frameRateThread
    liftIO $ killThread frameRateThread
    -- return final state
    liftIO $ pure s'
