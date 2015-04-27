{-# LANGUAGE OverloadedStrings #-}

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
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA (sha1, showDigest)
import Control.Exception (catch, throwIO, AssertionFailed(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T

tagXMPPConn :: String
tagXMPPConn = "XMPP.Conn"

-- |An XMPP connection over TCP.
data TCPConnection = TCPConnection { handle :: Handle
                                   , buffer :: IORef T.Text
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
                      [("to",T.pack serverName),
                       ("xmlns","jabber:client"),
                       ("xmlns:stream","http://etherx.jabber.org/streams")]
                      []
      debugM tagXMPPConn $ "Sending : "++T.unpack s
      T.hPutStr h s
      buffer <- newIORef T.empty
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
                      [("to", T.pack compName),
                       ("xmlns","jabber:component:accept"),
                       ("xmlns:stream","http://etherx.jabber.org/streams")]
                      []
      debugM tagXMPPConn $ "Sending : "++T.unpack s
      T.hPutStr h s
      buffer <- newIORef T.empty
      readLock <- newMVar ()
      writeLock <- newMVar ()
      let c = TCPConnection h buffer readLock writeLock -- Nothing
      e <- getStreamStart c
      debugM tagXMPPConn $ "Got : "++show e
      let from = maybe "" id (getAttr "from" e)
      let idStr = maybe "" id (getAttr "id" e)
      if from==T.pack compName && not (T.null idStr)
         then doHandshake c idStr secret
         else error "from mismatch"
      return c

  where
    doHandshake c idStr secret = do
      let digest = showDigest . sha1 . BL.pack . map (fromIntegral . ord) $ T.unpack idStr++secret
      debugM tagXMPPConn $ "digest="++digest
      sendStanza c $ XML "handshake" [] [CData $ T.pack digest]
      s <- getStanzas c
      debugM tagXMPPConn $ "got handshake response : "++show s

getSvcServer :: String -> Int -> IO [(String, PortID)]
getSvcServer domain port = return [(domain,PortNumber $ toEnum port)]

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
    hSetEncoding s utf8
    enc <- hGetEncoding s
    debugM tagXMPPConn $ "Connected, encoding : "++show enc
    return s

-- |Get the stream header that the server sent.  This needs to be
-- called before doing anything else with the stream.
getStreamStart :: TCPConnection -> IO XMLElem
getStreamStart c =
    parseBuffered c xmppStreamStart

withLock :: MVar () -> IO a -> IO a
withLock mvar a = withMVar mvar $ \_ -> a

instance XMPPConnection TCPConnection where
    getStanzas c = withLock (readLock c) $ do x <- parseBuffered c deepTag ; return [x]  -- FIXME
    sendStanza c x =
        let str = xmlToString True x
        in withLock (writeLock c) $ do
               debugM tagXMPPConn $ "sent '" ++ T.unpack str ++ "'"
               T.hPutStr (handle c) str
    closeConnection c =
        hClose (handle c)

parseBuffered :: (Show a) => TCPConnection -> Parser a -> IO a
parseBuffered c parser = do
    buf <- readIORef (buffer c)
    go (parse parser) buf

  where
    readMore = getString (handle c)

    -- go :: (T.Text -> IResult T.Text a) -> T.Text -> IO a
    go p buf1 = do
        buf <- if T.null buf1
                  then readMore
                  else return buf1
        debugM tagXMPPConn $ "got '" ++ T.unpack buf ++ "'"
        case p buf of
            Fail rest _ctxt msg -> do warningM tagXMPPConn $ "An error! Throwing exception : "++msg
                                      writeIORef (buffer c) rest
                                      throwIO (AssertionFailed "Protocol error")
                                      --parseBuffered c parser

            Done rest result -> do writeIORef (buffer c) rest
                                   return result

            Partial cont -> go cont =<< readMore

getString :: Handle -> IO T.Text
getString h = T.hGetChunk h

{-
debugLog debugH m = case debugH of
                      Nothing -> return ()
                      Just debugH -> hPutStr debugH m >> hFlush debugH
-}
