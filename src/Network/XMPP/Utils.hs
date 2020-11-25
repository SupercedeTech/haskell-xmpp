{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Utils
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Copyright   :  (c) riskbook, 2020
-- SPDX-License-Identifier:  BSD3
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Various XMPP\/XML utilities 
--
-----------------------------------------------------------------------------

module Network.XMPP.Utils ( debug, debugIO ) where

import           Control.Monad.IO.Class          (MonadIO)
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
