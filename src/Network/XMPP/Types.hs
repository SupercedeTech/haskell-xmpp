{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}


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
( Server, Username, Password, Resource
, XmppMonad, runXmppMonad, runXmppMonad'
, Stream(..), StreamType(..)
, Stanza(..), StanzaType(..), SomeStanza(..)
, MessageType(..), PresenceType(..), IQType(..), ShowType(..)
, RosterItem(..)
, JID(..), JIDQualification(..), SomeJID(..)
, StanzaPurpose(..)
, Sing(..), IncomingSym0, OutgoingSym0, SStanzaPurpose
)
where

import System.IO              (Handle, stdin)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (MonadState, StateT, runStateT)
import qualified Data.Text as T
import Data.Maybe             (maybeToList)

import Text.Blaze             (ToMarkup (toMarkup))
import Text.Regex
import Text.XML.HaXml.Types   (Content)
import Text.XML.HaXml.Posn    (Posn)
import Text.XML.HaXml.Lex     (Token)
import Text.XML               (Node)
import Data.Singletons.TH     (genSingletons, Sing(..))

--------------------------------------------------------------------------------

type Server   = String
type Username = String
type Password = String
type Resource = String

--------------------------------------------------------------------------------

-- | XMPP stream, used as a state in XmppMonad state transformer
data Stream
    = Stream {
      handle::Handle 
      -- ^ IO handle to the underlying file or socket
    , idx :: !Int
      -- ^ id of the next message (if needed)
    , lexemes :: [Token]
      -- ^ Stream of the lexemes coming from server
    }

newtype XmppMonad a
    = XmppMonad { unXmppMonad :: StateT Stream IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Stream)

runXmppMonad :: XmppMonad a -> IO (a, Stream)
runXmppMonad = flip runStateT newStream . unXmppMonad
    where
        newStream = Stream { handle  = stdin
                           , idx     = 0
                           , lexemes = []
                           }

runXmppMonad' :: Stream -> XmppMonad a -> IO (a, Stream)
runXmppMonad' s = flip runStateT s . unXmppMonad 

--------------------------------------------------------------------------------
-- | Jabber ID (JID) datatype
--
-- https://xmpp.org/extensions/xep-0029.html#sect-idm45723967532368
-- <JID>      - [<node>"@"]<domain>["/"<resource>]
-- <node>     - <conforming-char>[<conforming-char>]* - The node identifier (optional)
-- <domain>   - <hname>["."<hname>]*                  - The domain identifier (required)
-- <resource> - <any-char>[<any-char>]*               - The resource identifier (optional)

newtype DomainID = DomainID T.Text deriving (Eq, Show)

newtype NodeID = NodeID T.Text deriving (Eq, Show)

newtype ResourceID = ResourceID T.Text deriving (Eq, Show)

newtype JIDDomain = JIDDomain DomainID
  deriving (Eq, Show)

data JIDNodeResource = JIDNodeResource DomainID ResourceID
  deriving (Eq, Show)

data JIDResource = JIDResource DomainID ResourceID
  deriving (Eq, Show)

data JIDQualification
  = Resource
  | NodeResource
  | Domain

data SomeJID = forall (a :: JIDQualification). SomeJID (JID a)

data JID :: JIDQualification -> * where
  ResourceJID     :: { jrDomain :: DomainID      -- ^ Server adress
                     , jrResource :: ResourceID  -- ^ Resource name
                     } -> JID 'Resource

  NodeResourceJID :: { jnrNode :: NodeID           -- ^ Account name
                     , jnrDomain :: DomainID       -- ^ Server adress
                     , jnrResource :: ResourceID   -- ^ Resource name
                     } -> JID 'NodeResource

  DomainJID       :: { jdDomain :: DomainID } -> JID 'Domain -- ^ Server adress

instance Read (JID 'NodeResource) where
  readsPrec prev str =
    case readsPrec prev str of
      [(SomeJID j@NodeResourceJID{}, after)] -> [(j, after)]
      _ -> []

instance Read (JID 'Resource) where
  readsPrec prev str =
    case readsPrec prev str of
      [(SomeJID j@ResourceJID{}, after)] -> [(j, after)]
      _ -> []

instance Read (JID 'Domain) where
  readsPrec prev str =
    case readsPrec prev str of
      [(SomeJID j@DomainJID{}, after)] -> [(j, after)]
      _ -> []

instance Read SomeJID where
  -- Reads JID from string (name@server\/resource)
  readsPrec _ str = case matchRegexAll regex str of
    Just (_, _, after, [_, name, _, server, _, _, resource, _]) ->
      fmap (, after) . maybeToList $ case (toMaybe name, server, toMaybe resource) of
        (Just node, domain, Just resource) ->
          let nodeId     = NodeID $ T.pack node
              domainId   = DomainID $ T.pack domain
              resourceId = ResourceID $ T.pack resource
          in  Just $ SomeJID $ NodeResourceJID nodeId domainId resourceId
        (Nothing, domain, Nothing) ->
          Just $ SomeJID $ DomainJID $ DomainID $ T.pack domain
        (Nothing, domain, Just resource) ->
          let domainId   = DomainID $ T.pack domain
              resourceId = ResourceID $ T.pack resource
          in  Just $ SomeJID $ ResourceJID domainId resourceId
        _ -> Nothing
    _  -> []
    where
      toMaybe "" = Nothing
      toMaybe s  = Just s
      regex = mkRegex $ "((([^@])+)@)?" ++ "(([^/])+)" ++ "(/((.)+))?"

instance Show SomeJID where
  show (SomeJID j) = show j

instance Show (JID a) where
  show (NodeResourceJID (NodeID node) (DomainID domain) (ResourceID resource)) =
    T.unpack $ node <> "@" <> domain <> "/" <> resource
  show (ResourceJID (DomainID domain) (ResourceID resource)) =
    T.unpack $ domain <> "/" <> resource
  show (DomainJID (DomainID domain)) = T.unpack domain

instance ToMarkup (JID a) where
    toMarkup = toMarkup . show

--------------------------------------------------------------------------------

-- | XMPP Stream type, used in 'stream' pretty-printing combinator and the likes
data StreamType = Client -- ^ Client-to-server
                | ComponentAccept -- ^ FIXME
                | ComponentConnect -- ^ FIXME

instance Show StreamType where
  show Client = "jabber:client"
  show ComponentAccept = "jabber:component:accept"
  show ComponentConnect = "jabber:component:connect"

-- | Roster item type (7.1)
data RosterItem = RosterItem { jid :: JID 'NodeResource
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


--------------------------------------------------------------------------------

data MessageType
    = Chat
    | GroupChat
    | Headline
    | Normal
    | MessageError
    deriving (Eq)

instance Show MessageType where
  show Chat = "chat"
  show GroupChat = "groupchat"
  show Headline = "headline"
  show Normal = "normal"
  show MessageError = "error"
instance Read MessageType where
  readsPrec _ "chat" = [(Chat, "")]
  readsPrec _ "groupchat" = [(GroupChat, "")]
  readsPrec _ "headline" = [(Headline, "")]
  readsPrec _ "normal" = [(Normal, "")]
  readsPrec _ "error" = [(MessageError, "")]
  readsPrec _ "" = [(Chat, "")]
  readsPrec _ _ = error "incorrect message type"

data PresenceType
    = Default
    | Unavailable
    | Subscribe
    | Subscribed
    | Unsubscribe
    | Unsubscribed
    | Probe
    | PresenceError
    deriving (Eq)

instance Show PresenceType where
  show Default = ""
  show Unavailable = "unavailable"
  show Subscribe = "subscribe"
  show Subscribed = "subscribed"
  show Unsubscribe = "unsubscribe"
  show Unsubscribed = "unsubscribed"
  show Probe = "probe"
  show PresenceError = "error"
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

data IQType
    = Get
    | Result
    | Set
    | IQError
    deriving (Eq)

instance Show IQType where
  show Get = "get"
  show Result = "result"
  show Set = "set"
  show IQError = "error"
instance Read IQType where
  readsPrec _ "get" = [(Get, "")]
  readsPrec _ "result" = [(Result, "")]
  readsPrec _ "set" = [(Set, "")]
  readsPrec _ "error" = [(IQError, "")]
  readsPrec _ "" = [(Get, "")]
  readsPrec _ _ = error "incorrect iq type"

data ShowType = Available
  | Away
  | FreeChat
  | DND
  | XAway
  deriving (Eq)

instance Show ShowType where
  show Available = ""
  show Away = "away"
  show FreeChat = "chat"
  show DND = "dnd"
  show XAway = "xa"
instance Read ShowType where
  readsPrec _ "" = [(Available, "")]
  readsPrec _ "available" = [(Available, "")]
  readsPrec _ "away" = [(Away, "")]
  readsPrec _ "chat" = [(FreeChat, "")]
  readsPrec _ "dnd" = [(DND, "")]
  readsPrec _ "xa" = [(XAway, "")]
  readsPrec _ "invisible" = [(Available, "")]
  readsPrec _ _ = error "incorrect <show> value"

--------------------------------------------------------------------------------
-- | Generic XMPP stream atom

data StanzaPurpose = Incoming | Outgoing
  deriving (Eq, Show)

$(genSingletons [''StanzaPurpose])

data SomeStanza = forall (a :: StanzaType) p. SomeStanza (Stanza a p)

data StanzaType
    = Message
    | Presence
    | IQ


type family DataByPurpose (p :: StanzaPurpose) where
  DataByPurpose 'Incoming = [Content Posn]
  DataByPurpose 'Outgoing = [Node]

data Stanza :: StanzaType -> StanzaPurpose -> * where
    MkMessage ::
        { mFrom    :: Maybe (JID 'NodeResource)
        , mTo      :: SomeJID
        , mId      :: String -- ^ Message 'from', 'to', 'id' attributes
        , mType    :: MessageType -- ^ Message type (2.1.1)
        , mSubject :: String -- ^ Subject element (2.1.2.1)
        , mBody    :: String -- ^ Body element (2.1.2.2)
        , mThread  :: String -- ^ Thread element (2.1.2.3)
        , mExt     :: DataByPurpose p -- ^ Additional contents, used for extensions
        , mPurpose :: Sing p
        }
        -> Stanza 'Message p
    MkPresence ::
        { pFrom     :: Maybe SomeJID
        , pTo       :: Maybe SomeJID
        , pId       :: String -- ^ Presence 'from', 'to', 'id' attributes
        , pType     :: PresenceType -- ^ Presence type (2.2.1)
        , pShowType :: ShowType -- ^ Show element (2.2.2.1)
        , pStatus   :: String -- ^ Status element (2.2.2.2)
        , pPriority :: Maybe Integer -- ^ Presence priority (2.2.2.3)
        , pExt      :: DataByPurpose p -- ^ Additional contents, used for extensions
        , pPurpose :: Sing p
        }
        -> Stanza 'Presence p
    MkIQ ::
        { iqFrom  :: Maybe SomeJID
        , iqTo    :: Maybe SomeJID
        , iqId    :: String -- ^ IQ id (Core-9.2.3)
        , iqType  :: IQType -- ^ IQ type (Core-9.2.3)
        , iqBody  :: DataByPurpose p -- ^ Child element (Core-9.2.3)
        , iqPurpose :: Sing p
        }
        -> Stanza 'IQ p

instance Show (Sing 'Incoming) where
  show _ = "incoming"

instance Show (Sing 'Outgoing) where
  show _ = "outgoing"

deriving instance Show (Stanza t 'Incoming)
deriving instance Show (Stanza t 'Outgoing)
