module Network.XMPP.XMPPConnection (XMPPConnection(..)) where

import Network.XMPP.XMLParse

-- |A class for various kinds of XMPP connections.
class XMPPConnection c where
    -- |Get incoming stanzas from the connection.
    getStanzas :: c -> IO [XMLElem]
    -- |Send a stanza on the connection.
    sendStanza :: c -> XMLElem -> IO ()
    -- |Close the connection.
    closeConnection :: c -> IO ()
