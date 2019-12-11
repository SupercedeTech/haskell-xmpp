{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Types
-- Copyright   :  (c) Dmitry Astapov, 2006 ; pierre, 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Network.XMPP.Types
  ( XmppMessage
  , XmppStateT(runXmppStateT)
  , Stream(..), StreamType(..)
  , Stanza(..), StanzaType(..), SomeStanza(..)
  , MessageType(..), PresenceType(..), IQType(..), ShowType(..)
  , RosterItem(..)
  , defaultStreamBlockSize
  ) where

import System.IO (Handle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (MonadState, StateT)

import Text.XML.HaXml.Types (Content)
import Text.XML.HaXml.Posn (Posn)
import Text.XML.HaXml.Lex (Token)
    
import Text.PrettyPrint.HughesPJ (render)
import qualified Text.XML.HaXml.Pretty as P (content)

import Network.XMPP.JID

-- | XMPP message in the parsed form
type XmppMessage = Content Posn    

-- | XMPP stream, used as a state in XmppStateT state transformer
data Stream = Stream { handle::Handle 
                     -- ^ IO handle to the underlying file or socket
                     , idx :: !Int
                     -- ^ id of the next message (if needed)
                     , lexemes :: [Token]
                     -- ^ Stream of the lexemes coming from server
                     }

-- | Since XMPP is network-oriented, block size is equal to maximal MTU
defaultStreamBlockSize :: Int
defaultStreamBlockSize = 1500

-- | XmppStateT is a state transformer over IO monad, using Stream as a "state holder".
--  For API, look into 'Network.XMPP.Stream'
newtype XmppStateT a
    = XmppStateT { runXmppStateT :: StateT Stream IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Stream)

-- | XMPP Stream type, used in 'stream' pretty-printing combinator and the likes
data StreamType = Client -- ^ Client-to-server
                | ComponentAccept -- ^ FIXME
                | ComponentConnect -- ^ FIXME

instance Show StreamType where
  show Client = "jabber:client"
  show ComponentAccept = "jabber:component:accept"
  show ComponentConnect = "jabber:component:connect"

-- | Roster item type (7.1)
data RosterItem = RosterItem { jid :: JID
                             -- ^ Entry's JID
                             , subscribtion :: SubscribtionType
                             -- ^ Subscribtion type 
                             , nickname :: Maybe String
                             -- ^ Entry's nickname
                             , groups :: [String]
                             -- ^ <group> elements
                             }

data SubscribtionType = None | To | From | Both deriving Eq

instance Show SubscribtionType where
  show None = "none"
  show To = "to"
  show From = "from"
  show Both = "both"

instance Read SubscribtionType where
  readsPrec _ "none" = [(None, "")]
  readsPrec _ "to" = [(To, "")]
  readsPrec _ "from" = [(From, "")]
  readsPrec _ "both" = [(Both, "")]
  readsPrec _ "" = [(None, "")]                       
  readsPrec _ _ = error "incorrect subscribtion type"

-- | Generic XMPP stream atom
data SomeStanza = forall a. SomeStanza (Stanza a)

data StanzaType
    = Message
    | Presence
    | IQ

data Stanza :: StanzaType -> * where
    MkMessage :: { mFrom :: Maybe JID
                , mTo :: JID
                , mId :: String
                -- ^ Message 'from', 'to', 'id' attributes                              
                , mType :: MessageType
                -- ^ Message type (2.1.1)
                , mSubject :: String
                -- ^ Subject element (2.1.2.1)
                , mBody :: String
                -- ^ Body element (2.1.2.2)
                , mThread :: String
                -- ^ Thread element (2.1.2.3)
                , mExt :: [Content Posn]
                -- ^ Additional contents, used for extensions
                }
              -> Stanza 'Message
    MkPresence :: { pFrom :: Maybe JID
                  , pTo :: Maybe JID
                  , pId :: String
                  -- ^ Presence 'from', 'to', 'id' attributes
                  , pType :: PresenceType
                  -- ^ Presence type (2.2.1)
                  , pShowType :: ShowType
                  -- ^ Show element (2.2.2.1)
                  , pStatus :: String
                  -- ^ Status element (2.2.2.2)
                  , pPriority :: Maybe Integer
                  -- ^ Presence priority (2.2.2.3)
                  , pExt :: [Content Posn]
                  -- ^ Additional contents, used for extensions
                  }
               -> Stanza 'Presence
    MkIQ :: { iqFrom :: Maybe JID
            , iqTo :: Maybe JID
            , iqId :: String
            -- ^ IQ id (Core-9.2.3)
            , iqType :: IQType
            -- ^ IQ type (Core-9.2.3)
            , iqBody :: [Content Posn]
            -- ^ Child element (Core-9.2.3)
            }
         -> Stanza 'IQ

deriving instance Show (Stanza t)
              
data MessageType= Chat | GroupChat | Headline | Normal | MessageError deriving Eq
                 
data PresenceType = Default | Unavailable | Subscribe | Subscribed | Unsubscribe | Unsubscribed | Probe | PresenceError deriving Eq

data IQType = Get | Result | Set | IQError deriving Eq

data ShowType = Available | Away | FreeChat | DND | XAway deriving Eq

--instance Show (Content a) where
--  show = render . P.content

instance Show MessageType where
  show Chat = "chat"
  show GroupChat = "groupchat"
  show Headline = "headline"
  show Normal = "normal"
  show MessageError = "error"

instance Show PresenceType where
  show Default = ""
  show Unavailable = "unavailable"
  show Subscribe = "subscribe"
  show Subscribed = "subscribed"
  show Unsubscribe = "unsubscribe"
  show Unsubscribed = "unsubscribed"
  show Probe = "probe"
  show PresenceError = "error"

instance Show IQType where
  show Get = "get"
  show Result = "result"
  show Set = "set"
  show IQError = "error"

instance Show ShowType where
  show Available = ""
  show Away = "away"
  show FreeChat = "chat"
  show DND = "dnd"
  show XAway = "xa"

instance Read MessageType where
  readsPrec _ "chat" = [(Chat, "")]
  readsPrec _ "groupchat" = [(GroupChat, "")]
  readsPrec _ "headline" = [(Headline, "")]
  readsPrec _ "normal" = [(Normal, "")]
  readsPrec _ "error" = [(MessageError, "")]
  readsPrec _ "" = [(Chat, "")]                        
  readsPrec _ _ = error "incorrect message type"

instance Read PresenceType where
  readsPrec _ "" = [(Default, "")]
  readsPrec _ "available" = [(Default, "")]
  readsPrec _ "unavailable" = [(Unavailable, "")]
  readsPrec _ "subscribe" = [(Subscribe, "")]
  readsPrec _ "subscribed" = [(Subscribed, "")]
  readsPrec _ "unsubscribe" = [(Unsubscribe, "")]
  readsPrec _ "unsubscribed" = [(Unsubscribed, "")]
  readsPrec _ "probe" = [(Probe, "")]
  readsPrec _ "error" = [(PresenceError, "")]
  readsPrec _ _ = error "incorrect presence type"
                    
instance Read IQType where
  readsPrec _ "get" = [(Get, "")]
  readsPrec _ "result" = [(Result, "")]
  readsPrec _ "set" = [(Set, "")]
  readsPrec _ "error" = [(IQError, "")]
  readsPrec _ "" = [(Get, "")]
  readsPrec _ _ = error "incorrect iq type"

instance Read ShowType where
  readsPrec _ "" = [(Available, "")]
  readsPrec _ "available" = [(Available, "")]
  readsPrec _ "away" = [(Away, "")]
  readsPrec _ "chat" = [(FreeChat, "")]
  readsPrec _ "dnd" = [(DND, "")]
  readsPrec _ "xa" = [(XAway, "")]                        
  readsPrec _ "invisible" = [(Available, "")]
  readsPrec _ _ = error "incorrect <show> value"

