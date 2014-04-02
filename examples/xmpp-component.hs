#!/usr/bin/env runghc

import System.Log.Logger hiding (addHandler)
import Network.XMPP
import Data.Maybe

compDomain = "my-component.example.com"

compPort = 8888
compPasswd = "secret"
xmppAddr = "127.0.0.1"

main = do
  updateGlobalLogger "XMPP" $ setLevel DEBUG
  updateGlobalLogger "XMPP.Monad" $ setLevel WARNING
  c <- openComponent xmppAddr compPort compDomain compPasswd
  runXMPP c $ do
         addHandler (const True) hndlr True
         addHandler isIQReq handlePing True

hndlr e = do
  liftIO $ putStrLn $ "Received unknown : "++show e

isIQReq :: XMLElem -> Bool
isIQReq x = isIq x && getAttr "type" x == Just "get"

handlePing :: XMLElem -> XMPP ()
handlePing x = do
    sendIqResp x "result" [XML "query" [] []]
    return ()
  where
    from = fromMaybe (error "missing from!") $ getAttr "from" x




sendIqResp inResponseTo iqtype payload =
    sendStanza $ XML "iq"
                   [("to", from),
                    ("from", to),
                    ("type", iqtype),
                    ("id", id)]
                   payload
  where
    from = fromMaybe (error "missing from!") $ getAttr "from" inResponseTo
    to = fromMaybe (error "missing to!") $ getAttr "to" inResponseTo
    id = fromMaybe (error "missing id!") $ getAttr "id" inResponseTo
