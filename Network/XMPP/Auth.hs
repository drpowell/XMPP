module Network.XMPP.Auth where

import Network.XMPP.XMLParse
import Network.XMPP.XMPPMonad
import Network.XMPP.Stanzas
import Network.XMPP.MyDebug

-- |Non-SASL authentication, following XEP-0078.
startAuth :: String             -- ^Username (part before \@ in JID)
          -> String             -- ^Server (part after \@ in JID)
          -> String             -- ^Password
          -> String             -- ^Resource (unique identifier for this connection)
          -> XMPP Integer       -- ^Error number. Zero if authentication succeeded.
startAuth username server password resource = do
  response <- sendIqWait server "get" [XML "query"
                                       [("xmlns","jabber:iq:auth")]
                                       [XML "username"
                                        []
                                        [CData username]]]
  case xmlPath ["query","password"] response of
    Nothing -> return 1 -- plaintext authentication not supported by server
                        -- http://xmpp.org/extensions/attic/jep-0078-1.7.html
                        -- "If there is no such username, the server SHOULD NOT return an error"
                        -- So server can return error here, if username is wrong.
    Just _ -> do
      response' <- sendIqWait server "set" [XML "query"
                                            [("xmlns","jabber:iq:auth")]
                                            [XML "username" []
                                                     [CData username],
                                             XML "password" []
                                                     [CData password],
                                             XML "resource" []
                                                     [CData resource]]]
      return $ getErrorCode response'
