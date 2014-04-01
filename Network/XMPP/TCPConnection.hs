module Network.XMPP.TCPConnection
                     ( TCPConnection
                     , openStream
                     , getStreamStart
                     , openComponent
                     , tagXMPPConn
                     )
    where

import Network.XMPP.XMLParse
import Network.XMPP.XMPPConnection
import System.Log.Logger

import Network
import Control.Concurrent.MVar
import System.IO
import Data.IORef
import Control.Monad
import Codec.Binary.UTF8.String
import ADNS
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.SHA
import Control.Exception (catch)

tagXMPPConn :: String
tagXMPPConn = "XMPP.Conn"

-- |An XMPP connection over TCP.
data TCPConnection = TCPConnection { handle :: Handle
                                   , buffer :: IORef String
                                   , readLock :: MVar ()
                                   , writeLock :: MVar ()
                                   --, debugFile :: Maybe Handle
                                   }

-- |Open a TCP connection to the named server, port 5222 (or others
-- found in SRV), and send a stream header.
openStream :: String -> String -> IO TCPConnection
openStream server serverName =
    do
      -- here we do service lookup (via SRV or A)
      svcs <- getSvcServer server 5222

      h <- connectStream svcs
      let s = xmlToString False $
              XML "stream:stream"
                      [("to",serverName),
                       ("xmlns","jabber:client"),
                       ("xmlns:stream","http://etherx.jabber.org/streams")]
                      []
      debugM tagXMPPConn $ "Sending : "++s
      hPutStr h s
      buffer <- newIORef ""
      readLock <- newMVar ()
      writeLock <- newMVar ()
      --debugFile <- openFile ("xx-"++show h) WriteMode
      return $ TCPConnection h buffer readLock writeLock -- (Just debugFile)

openComponent :: String -> Int -> String -> String -> IO TCPConnection
openComponent server port compName secret =
    do
      svcs <- getSvcServer server port
      h <- connectStream svcs
      let s = xmlToString False $
              XML "stream:stream"
                      [("to", compName),
                       ("xmlns","jabber:component:accept"),
                       ("xmlns:stream","http://etherx.jabber.org/streams")]
                      []
      debugM tagXMPPConn $ "Sending : "++s
      hPutStr h s
      buffer <- newIORef ""
      readLock <- newMVar ()
      writeLock <- newMVar ()
      let c = TCPConnection h buffer readLock writeLock -- Nothing
      e <- getStreamStart c
      debugM tagXMPPConn $ "Got : "++show e
      let from = maybe "" id (getAttr "from" e)
      let idStr = maybe "" id (getAttr "id" e)
      if from==compName && not (null idStr)
         then doHandshake c idStr secret
         else error "from mismatch"
      return c

  where
    doHandshake c idStr secret = do
      let digest = showDigest . sha1 . L.pack . encode $ idStr++secret
      debugM tagXMPPConn $ "digest="++digest
      sendStanza c $ XML "handshake" [] [CData digest]
      s <- getStanzas c
      debugM tagXMPPConn $ "got handshake response : "++show s

getSvcServer :: String -> Int -> IO [(String, PortID)]
getSvcServer domain port = return [(domain,PortNumber $ toEnum port)]
{-
    initResolver [] $ \resolver -> do
        a <- querySRV resolver ("_xmpp-client._tcp." ++ domain)
        return $ (maybe [] id a) ++ [(domain, PortNumber $ toEnum port)]
-}


connectStream :: [(String, PortID)] -> IO Handle
connectStream [] = error "can't connect: no suitable servers found"
connectStream (x:xs) =
    Control.Exception.catch
               (connectStream' x)
               (\e -> putStrLn ("e="++show (e :: IOError)) >> connectStream xs)

connectStream' :: (String, PortID) -> IO Handle
connectStream' (host, port) = do
    debugM tagXMPPConn $ "Trying connectTo : "++host -- ++" : "++show port
    s <- connectTo host port
    hSetBuffering s NoBuffering
    return s

-- |Get the stream header that the server sent.  This needs to be
-- called before doing anything else with the stream.
getStreamStart :: TCPConnection -> IO XMLElem
getStreamStart c =
    parseBuffered c xmppStreamStart

withLock :: MVar () -> IO a -> IO a
withLock mvar a = withMVar mvar $ \_ -> a

instance XMPPConnection TCPConnection where
    getStanzas c = withLock (readLock c) $ parseBuffered c deepTags
    sendStanza c x =
        let str = xmlToString True x
        in withLock (writeLock c) $ do
               debugM tagXMPPConn $ "sent '" ++ str ++ "'"
               hPutStr (handle c) (encodeString str)
    closeConnection c =
        hClose (handle c)

parseBuffered :: TCPConnection -> Parser a -> IO a
parseBuffered c parser = do
  buf <- readIORef (buffer c)
  input' <- getString (handle c)
  let input = decodeString input'
  debugM tagXMPPConn $ "got '" ++ buf ++ input ++ "'"
  case parse (getRest parser) "" (buf++input) of
    Right (result, rest) ->
        do
          writeIORef (buffer c) rest
          return result
    Left e ->
        do
          warningM tagXMPPConn $ "An error?  Hopefully doesn't matter."++(show e)
          parseBuffered c parser

getString :: Handle -> IO String
getString h =
    do
      eof <- hIsEOF h
      when (not eof) $
         hWaitForInput h (-1) >> return ()
      getEverything
    where getEverything =
              do
                r <- hReady h
                if r
                  then liftM2 (:) (hGetChar h) getEverything
                  else return []

{-
debugLog debugH m = case debugH of
                      Nothing -> return ()
                      Just debugH -> hPutStr debugH m >> hFlush debugH
-}
