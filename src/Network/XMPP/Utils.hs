{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Utils
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Various XMPP\/XML utilities 
--
-----------------------------------------------------------------------------

module Network.XMPP.Utils ( debug, debugIO ) where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Network.XMPP.Types

debug :: MonadIO m => String -> XmppMonad m ()
debugIO :: String -> IO ()
#ifdef DEBUG
debug = liftIO . putStrLn
debugIO = putStrLn
#else
debug _ = return ()
debugIO _ = return ()
#endif
