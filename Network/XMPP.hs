-- |This library aims to make writing XMPP clients (in particular
-- bots) easy and fun.  Here is a small example:
--
-- > import Network
-- > import Network.XMPP
-- >
-- > -- The bot's JID is "bot@example.com"
-- > botUsername = "bot"
-- > botServer = "example.com"
-- > botPassword = "secret"
-- > botResource = "bot"
-- >
-- > main :: IO ()
-- > main = withSocketsDo $
-- >   do
-- >     -- Connect to server...
-- >     c <- openStream botServer
-- >     getStreamStart c
-- >
-- >     runXMPP c $ do
-- >       -- ...authenticate...
-- >       startAuth botUsername botServer botPassword botResource
-- >       sendpresence Nothing Nothing
-- >       -- ...and do something.
-- >       run
-- >
-- > run :: XMPP ()
-- > run = do
-- >   -- Wait for an incoming message...
-- >   msg <- waitForStanza (isChat `conj` hasBody)
-- >   let sender = maybe "" id (getAttr "from" msg)
-- >       len = length $ maybe "" id (getMessageBody msg)
-- >   -- ...answer...
-- >   sendMessage sender ("Your message was "++(show len)++" characters long.")
-- >   -- ...and repeat.
-- >   run
--
-- XMPP is a protocol for streaming XML also known as Jabber.  It is
-- described in RFCs 3920 and 3921, and in a series of XMPP Extension
-- Protocols (XEPs).  All of this can be found at
-- <http://www.xmpp.org>.
module Network.XMPP (
              -- * The XMPP monad
              module Network.XMPP.XMPPMonad
              -- * XML functions
            , XMLElem(..)
            , xmlPath
            , xmlPath'
            , getAttr
            , getCdata
            , allChilds
            , xmlToString
              -- * Stanza manipulation
            , module Network.XMPP.Stanzas
              -- * JID functions
            , module Network.XMPP.JID
              -- * Authentication
            , module Network.XMPP.Auth
              -- * TCP connections
            , module Network.XMPP.TCPConnection
              -- * Abstract connections
            , module Network.XMPP.XMPPConnection
              -- * Roster management
            , module Network.XMPP.Roster
            )
    where

import Network.XMPP.Auth
import Network.XMPP.JID
import Network.XMPP.Stanzas
import Network.XMPP.TCPConnection
import Network.XMPP.XMLParse
import Network.XMPP.XMPPConnection hiding ( sendStanza )
import Network.XMPP.XMPPMonad
import Network.XMPP.Roster
