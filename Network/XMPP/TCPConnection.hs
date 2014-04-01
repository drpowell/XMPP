module Network.XMPP.TCPConnection
                     ( TCPConnection
                     , openStream
                     , getStreamStart
                     )
    where

import Network.XMPP.XMLParse
import Network.XMPP.XMPPConnection
import Network.XMPP.MyDebug

import Network
import System.IO
import Data.IORef
import Control.Monad
import Codec.Binary.UTF8.String
import ADNS


-- |An XMPP connection over TCP.
data TCPConnection = TCPConnection Handle (IORef String)

-- |Open a TCP connection to the named server, port 5222 (or others
-- found in SRV), and send a stream header.
openStream :: String -> IO TCPConnection
openStream server =
    do
      -- here we do service lookup (via SRV or A)
      svcs <- getSvcServer server

      h <- connectStream svcs
      hPutStr h $ xmlToString False $
              XML "stream:stream"
                      [("to",server),
                       ("xmlns","jabber:client"),
                       ("xmlns:stream","http://etherx.jabber.org/streams")]
                      []
      buffer <- newIORef ""
      return $ TCPConnection h buffer

getSvcServer :: String -> IO [(String, PortID)]
getSvcServer domain =
    initResolver [] $ \resolver -> do
        a <- querySRV resolver ("_xmpp-client._tcp." ++ domain)
        return $ (maybe [] id a) ++ [(domain, PortNumber $ toEnum 5222)]

connectStream :: [(String, PortID)] -> IO Handle
connectStream [] = error "can't connect: no suitable servers found"
connectStream (x:xs) =
    catch (connectStream' x) (\e -> connectStream xs)

connectStream' :: (String, PortID) -> IO Handle
connectStream' (host, port) = do
    s <- connectTo host port
    hSetBuffering s NoBuffering
    return s

-- |Get the stream header that the server sent.  This needs to be
-- called before doing anything else with the stream.
getStreamStart :: TCPConnection -> IO XMLElem
getStreamStart c =
    parseBuffered c xmppStreamStart

instance XMPPConnection TCPConnection where
    getStanzas c = parseBuffered c deepTags
    sendStanza (TCPConnection h _) x =
        let str = xmlToString True x in
        do
          myDebug $ "sent '" ++ str ++ "'"
          hPutStr h (encodeString str)
    closeConnection (TCPConnection h _) =
        hClose h

parseBuffered :: TCPConnection -> Parser a -> IO a
parseBuffered c@(TCPConnection h bufvar) parser = do
  buffer <- readIORef bufvar
  input' <- getString h
  let input = decodeString input'
  myDebug $ "got '" ++ buffer ++ input ++ "'"
  case parse (getRest parser) "" (buffer++input) of
    Right (result, rest) ->
        do
          writeIORef bufvar rest
          return result
    Left e ->
        do
          myDebug $ "An error?  Hopefully doesn't matter.\n"++(show e)
          parseBuffered c parser

getString :: Handle -> IO String
getString h =
    do
      hWaitForInput h (-1)
      getEverything
    where getEverything =
              do
                r <- hReady h
                if r
                  then liftM2 (:) (hGetChar h) getEverything
                  else return []
