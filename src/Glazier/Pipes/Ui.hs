{-# LANGUAGE RankNTypes #-}

module Glazier.Pipes.Ui where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar.Extras as STE
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import qualified Pipes as P
import qualified Pipes.Prelude as PP

-- | This is similar to part of the Elm startApp.
-- This is responsible for running the Glazier Gadget update tick until it quits.
-- This is also responsible for rendering the frame.
-- This function is only required if you are not using another application framework which
-- already takes care of rendering and processing user input.
runUi :: (MonadIO io) =>
     Int
  -> (s -> IO ()) -- render
  -> P.Producer s io s
  -> io s
runUi refreshDelay render appSignal = do
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
    latestState <- liftIO newEmptyTMVarIO
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

    -- application run
    s' <- P.runEffect $
        appSignal P.>-> PP.mapM
            (liftIO . atomically . void . STE.forceSwapTMVar latestState) P.>-> PP.drain

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
