{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad.Trans (liftIO)

import Network.XMPP
import Network.XMPP.IQ
import Network.XMPP.XEP.MUC
import Network.XMPP.XEP.Version
import Network.XMPP.Concurrent
import Control.Monad
-- import qualified Text.XML.HaXml.Pretty as P (content)
-- import Text.XML.HaXml.Xtract.Parse (xtract)

iqVersionT1 :: XmppThreadT ()
iqVersionT1 = do
  st <- readChanS
  liftIO $ putStrLn "thread1 got next message."
  case st of
    SomeStanza iq@MkIQ {} ->
      when (isVersionReq iq) $ do
        writeChanS $ SomeStanza $ presAway "thread1"
        liftIO $ putStrLn "thread1: it was version request. We sent away presence \"thread1\""
    _ -> pure ()
  iqVersionT1

iqVersionT2 :: XmppThreadT ()
iqVersionT2 = do
  st <- readChanS
  liftIO $ putStrLn "thread2 got next message."
  case st of
    SomeStanza iq@MkIQ {} ->
      when (isVersionReq iq) $ do
        writeChanS $ SomeStanza $ presAway "thread2"
        liftIO $ putStrLn "thread1: it was version request. We sent away presence \"thread2\""
    _ -> pure ()
  iqVersionT2

iqVersion :: XmppThreadT ()
iqVersion =
    loop $ iqReplyTo isVersionReq $ versionAnswer "Network.XMPP test" version "Linux"

main :: IO ()
main =
 do let user = "testbot"
    let pass = "testing"
    let server = "xmpp.org.ru"
    let resource = "haskell-xmpp-devel"
    let _room = "testing@conference.jabber.ru"
    void $ runXmppMonad $
      do h <- liftIO $ connectViaTcp server 5222
         jid <- initiateStream h server user pass resource
         liftIO $ putStrLn $ "My jid is " ++ show jid
         outStanza $ presAvailable "Hello, world!"
         runThreaded $ do
           void $ withNewThread iqVersion
           void $ withNewThread iqVersionT2
{-         
         -- Query and dump roster
         --out $ iq ! [ id_ "roster-get", type_ "get" ] << query_ "jabber:iq:roster"
         --roster <- xtractM "/iq[@type='result' & @id='roster-get']/query"
         --liftIO $ do putStrLn "Your roster:"
         --            putStrLn $ show $ map P.content roster
         out $ iq ! [ id_ "roster-get", type_ "get" ] << query_ "jabber:iq:roster"
         roster <- parseM
         liftIO $ putStrLn $ show roster
-}
         -- Set presence to default 

         outStanza $ enterRoom (read "testing@conference.jabber.ru/testbot") undefined
         outStanza $ leaveRoom (read "testing@conference.jabber.ru/testbot") undefined
{-                              
         -- Sit in MUC room, echoing all messages
         -- We echo all messages sent to MUC room or private chat,
         -- except messages sent by us and history messages sent upon entering the room
         let addr_sel = concat [ "@to=",show jid," & @from!=", show (room++"/"++user) ]
         -- FIXME: For some reason, negated conditions will have to be last in the xtract query, otherwise
         -- it behaves in odd way
         let all_msgs_to_me_xtract = concat [ "/message[", addr_sel," & @type='groupchat' & ~(x/@xmlns='jabber:x:delay')]"]
         liftIO $ putStrLn $ "Will select messages with filter: " ++ all_msgs_to_me_xtract

         let plugins = [ Plugin all_msgs_to_me_xtract (echo room)
                       , Plugin (concat ["/message[",addr_sel," & @type='chat' & ~(x/@xmlns='jabber:x:delay')]"]) privecho
                       , Plugin "/iq[@type='get']/query[@xmlns='jabber:iq:version']" iq_version
                       ]

         loopWithPlugins plugins
-}

{-
echo room m = 
  do let text = getText_ $ xtract id "/message/body/-" m
     if ("lambdabot:" `isPrefixOf` text)
        then do i <- getNextId
                out $ message ! [ to room, type_ "groupchat", id_ (show i), xmllang "en" ] << body_ << ("Echo: " ++ text)
        else return ()

privecho m = 
  do let text = getText_ $ xtract id "/message/body/-" m
     let sender = getText_ $ xtract id "/message/@from" m
     i <- getNextId
     out $ message ! [ to sender, type_ "chat", id_ (show i), xmllang "en" ] << body_ << ("Echo: " ++ text)

iq_version m =
  do let sender = getText_ $ xtract id "/iq/@from" m
     let i      = getText_ $ xtract id "/iq/@id" m
     out $ iq ! [ id_ i , to sender, type_ "result", xmllang "en" ]
              << fullquery "jabber:iq:version" << ( name_ << "lambdabot"
                                                    +++ version_ << "0.1"
                                                    +++ os << "Debian GNU/Linux testing/unstable 2.6.16-1-686" )
-}

{-
IN(1,adept@jabber.kiev.ua/work):
<iq from='devel@conference.jabber.ru/sulci'
	to='adept@jabber.kiev.ua/work'
	id='stoat_10787'
	type='get'>
  <query xmlns='jabber:iq:version'/>
</iq>
OUT(1,adept@jabber.kiev.ua/work):
<iq id='stoat_10787'
	to='devel@conference.jabber.ru/sulci'
	type='result'
	xml:lang='en'>
  <query xmlns='jabber:iq:version'>
    <name>Tkabber</name>
    <version>0.9.8-alpha-20060521 (Tcl/Tk 8.4.12)</version>
    <os>Debian GNU/Linux testing/unstable 2.6.16-1-686</os>
  </query>
</iq>
-}
