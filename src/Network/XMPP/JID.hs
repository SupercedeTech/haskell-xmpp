-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.JID
-- Copyright   :  (c) pierre, 2006
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- JID datatype and functions
--
-----------------------------------------------------------------------------


-- jid components and functions
-- probably import Stringprep

module Network.XMPP.JID
  ( JID(..)
  ) where

import Text.Regex

-- | Jabber ID (JID) datatype
data JID = JID { name :: Maybe String
               -- ^ Account name
               , server :: String
               -- ^ Server adress
               , resource :: Maybe String
               -- ^ Resource name
               }

instance Read JID where
    -- Reads JID from string (name@server\/resource)
    readsPrec _ str = case matchRegexAll regex str of                  
                        Just (_,_,after,(_:name:_:server:_:_:resource:_:[])) -> [((JID (toMaybe name) server (toMaybe resource)), after)]
                        Just _ -> []
                        Nothing -> []
        where
          toMaybe "" = Nothing
          toMaybe s  = Just s
          regex = mkRegex $ 
                  "((([^@])+)@)?" ++
                  "(([^/])+)" ++
                  "(/((.)+))?"

instance Show JID where
    -- Shows JID
  show jid = concat [name', server jid, resource']
    where name' = maybe "" (++"@") (name jid)
          resource' = maybe "" ("/"++) (resource jid)
    
    
