#!/usr/bin/env runghc

import System.Log.Logger hiding (addHandler)
import Network.XMPP
import Data.Maybe
import Control.Monad
import Control.Concurrent (forkIO,threadDelay)

compDomain = "my-component.example.com"
xmppHost = "localhost"
serverAddr = "127.0.0.1"

user = "username"
passwd = "password"
resource = "res"

main = do
  updateGlobalLogger "XMPP" $ setLevel DEBUG
  updateGlobalLogger "XMPP.Conn" $ setLevel DEBUG
  updateGlobalLogger "XMPP.Monad" $ setLevel WARNING
  conn <- openStream serverAddr xmppHost
  getStreamStart conn
  runXMPP conn $ do
      ok <- startAuth user xmppHost passwd resource
      -- liftIO $ putStrLn "auth done!"
      when (ok /= 0) $
          error $ "Authentication failed : "++show ok

  forkIO $ forever $ do
      threadDelay $ 1000 * 1000
      runXMPP conn (sendIq compDomain "get" [XML "query" [] []] >> return ())

  runXMPP conn $ do
         addHandler (const True) hndlr True

hndlr e = do
  liftIO $ putStrLn $ "Received : "++show e
