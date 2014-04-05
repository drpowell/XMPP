{-# LANGUAGE OverloadedStrings #-}
module Network.XMPP.Stanzas
               ( sendIq
               , sendIqWait
               , sendIqResponse
               , hasBody
               , getMessageBody
               , sendMessage
               , sendPresence
               , conj
               , attributeMatches
               , isMessage
               , isPresence
               , isIq
               , isChat
               , isFrom
               , iqXmlns
               , iqGet
               , iqSet
               , iqError
               , iqResult
               , handleVersion
               , getErrorCode
               , hasNodeName
               , getMessageStamp
               , getJidRes
               , cdata
               , cdata'
               )
    where

import Network.XMPP.XMPPMonad
import Network.XMPP.XMLParse
import Network.XMPP.JID
import System.Random
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

--- Sending info requests and responses

-- |Send an IQ request, returning the randomly generated ID.
sendIq :: Text                -- ^JID of recipient
       -> Text                -- ^Type of IQ, either \"get\" or \"set\"
       -> [XMLElem]             -- ^Payload elements
       -> XMPP Text         -- ^ID of sent stanza
sendIq to iqtype payload =
    do
      iqid <- liftIO $ (randomIO::IO Int)
      sendStanza $ XML "iq"
                     [("to", to),
                      ("type", iqtype),
                      ("id", T.pack $ show iqid)]
                     payload
      return $ T.pack $ show iqid

-- |Send an IQ request and wait for the response, without blocking
-- other activity.
sendIqWait :: Text            -- ^JID of recipient
           -> Text            -- ^Type of IQ, either \"get\" or \"set\"
           -> [XMLElem]         -- ^Payload elements
           -> XMPP XMLElem      -- ^Response stanza
sendIqWait to iqtype payload =
    do
      iqid <- sendIq to iqtype payload
      waitForStanza $ (hasNodeName "iq") `conj` (attributeMatches "id" (==iqid))

-- |Send a response to a received IQ stanza.
sendIqResponse :: XMLElem       -- ^Original stanza, from which id and
                                -- recipient are taken
               -> Text        -- ^Type of response, either
                                -- \"result\" or \"error\"
               -> [XMLElem]     -- ^Payload elements
               -> XMPP (Maybe ())   -- ^Just () if original stanza had
                                    -- a \"from\" attribute
sendIqResponse inResponseTo iqtype payload =
      case getAttr "from" inResponseTo of
        Nothing ->
            -- "from" attribute missing?
            return Nothing
        Just sender ->
            let iqid = maybe "" id (getAttr "id" inResponseTo)
            in do
                sendStanza $ XML "iq"
                               [("to", sender),
                                ("type", iqtype),
                                ("id", iqid)]
                               payload
                return $ Just ()

--- Messages

-- |Return true if the message stanza has body text.
hasBody :: StanzaPredicate
hasBody stanza = isJust $ getMessageBody stanza

-- |Get the body text of the message stanza, if any.
getMessageBody :: XMLElem -> Maybe Text
getMessageBody stanza =
    do
      bodyTag <- xmlPath ["body"] stanza
      getCdata bodyTag

-- |Send an ordinary \"chat\" type message.
sendMessage :: Text           -- ^JID of recipient
            -> Text           -- ^Text of message
            -> XMPP ()
sendMessage to body =
    sendStanza $ XML "message"
                   [("to", to),
                    ("type", "chat")]
                   [XML "body" []
                        [CData body]]

--- Presence

-- |Send ordinary online presence.
sendPresence :: Maybe (Text, [Text]) -> Maybe Integer -> XMPP ()
sendPresence status priority =
    sendStanza $ XML "presence" [] (status'++priority')
  where
    priority' = maybe [] (\p -> [XML "priority" [] [CData $ T.pack $ show p]]) priority
    status' = case status of
        Just (sh, ss) -> (XML "show" [] [CData sh]):(statuses ss)
        _             -> []
    statuses = map (\s -> XML "status" [] [CData s])

--- Stanza predicates

-- |Conjunction (\"and\") of two predicates.
conj :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
conj a b = \x -> a x && b x

-- |Return true if the tag has the given name.
hasNodeName :: Text -> StanzaPredicate
hasNodeName name (XML name' _ _) = name == name'
hasNodeName _ _ = error "Unexpected use of hasNodeName"

-- The three basic stanza types

-- |Return true if the tag is a message stanza.
isMessage :: StanzaPredicate
isMessage = hasNodeName "message"

-- |Return true if the tag is a presence stanza.
isPresence :: StanzaPredicate
isPresence = hasNodeName "presence"

-- |Return true if the tag is an IQ stanza.
isIq :: StanzaPredicate
isIq = hasNodeName "iq"

-- |Return true if the tag is a chat message.
isChat :: StanzaPredicate
isChat = isMessage `conj` attributeMatches "type" (=="chat")

-- |Apply the predicate to the named attribute.  Return false if the
-- tag has no such attribute.
attributeMatches :: Text      -- ^Attribute name
                 -> (Text -> Bool) -- ^Attribute value predicate
                 -> StanzaPredicate
attributeMatches attr p (XML _ attrs _) =
    maybe False p (lookup attr attrs)
attributeMatches _ _ _ = error "Unexpected use of attributeMatches"

-- |Return true if the stanza is from the given JID.
isFrom :: Text -> StanzaPredicate
isFrom jid = attributeMatches "from" (==jid)

-- |Return true if the stanza is an IQ stanza in the given namespace.
-- FIXME: query node not nessesary the first node in the iq stanza.
iqXmlns :: Text -> StanzaPredicate
iqXmlns xmlns (XML "iq" _ els) =
    case listToMaybe [x | x <- els, case x of
                                       XML _ _ _ -> True
                                       _ -> False] of
      Just x ->
          attributeMatches "xmlns" (==xmlns) x
      Nothing ->
          False
iqXmlns _ _ = False

-- |Return true if the stanza is a \"get\" request in the given namespace.
iqGet :: Text -> StanzaPredicate
iqGet xmlns = (attributeMatches "type" (=="get")) `conj` (iqXmlns xmlns)

-- |Return true if the stanza is a \"set\" request in the given namespace.
iqSet :: Text -> StanzaPredicate
iqSet xmlns = (attributeMatches "type" (=="set")) `conj` (iqXmlns xmlns)

-- |Return true if the stanza is a \"error\" request in the given namespace.
iqError :: Text -> StanzaPredicate
iqError xmlns = (attributeMatches "type" (=="error")) `conj` (iqXmlns xmlns)

-- |Return true if the stanza is a \"result\" request in the given namespace.
iqResult :: Text -> StanzaPredicate
iqResult xmlns = (attributeMatches "type" (=="result")) `conj` (iqXmlns xmlns)

--- Handlers for common requests

-- |Establish a handler for answering to version requests with the
-- given information.  See XEP-0092: Software Version.
handleVersion :: Text         -- ^Client name
              -> Text         -- ^Client version
              -> Text         -- ^Operating system
              -> XMPP ()
handleVersion name version os =
    addHandler (iqGet "jabber:iq:version")
               (\stanza ->
                    do
                      sendIqResponse stanza "result"
                                     $ [XML "query"
                                             [("xmlns", "jabber:iq:version")]
                                             [XML "name" [] [CData name],
                                              XML "version" [] [CData version],
                                              XML "os" [] [CData os]]]
                      return ())
               True

-- |Return stanza's error code or -1 (if can't parse error node).
-- Zero if no error.
getErrorCode :: XMLElem -> Integer
getErrorCode stanza =
    case getAttr "type" stanza of
      Just "error" -> read . T.unpack $ maybe "-1" id (getAttr "code" errorNode) :: Integer
      _ -> 0
    where errorNode = maybe (XML T.empty [] []) id (xmlPath ["error"] stanza)

-- |Get the stamp of message, if has any.
getMessageStamp :: XMLElem -> Maybe Text
getMessageStamp stanza =
    let xs = xmlPath' ["x"] [stanza]
        xs' = filter (attributeMatches "xmlns" (=="jabber:x:delay")) xs
        stamps = map (getAttr "stamp") xs'
    in case stamps of
         [stamp@(Just _s)] -> stamp
         _ -> Nothing

-- |Get the jid and the resource of stanza.
getJidRes :: XMLElem -> (Text, Text)
getJidRes stanza =
    let sender = maybe "" id (getAttr "from" stanza)
    in (getBareJid sender, getResource sender)

-- |Get cdata from xmlelem.
cdata :: XMLElem -> Text
cdata = maybe "" id . getCdata

-- |Get maybe cdata from maybe xmlelem.
cdata' :: Maybe XMLElem -> Maybe Text
cdata' = getCdata . maybe (XML T.empty [] []) id
