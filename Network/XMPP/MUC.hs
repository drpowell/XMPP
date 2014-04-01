-- |Implementation of Multi-User Chat, according to XEP-0045.  This
-- API needs more thought and will change.
module Network.XMPP.MUC where

import Network.XMPP

-- |Return true if the stanza is from a JID whose \"username\@server\"
-- part matches the given string.
matchesBare :: String -> StanzaPredicate
matchesBare bare = attributeMatches "from" ((==bare).getBareJid)

-- |Join groupchat.
joinGroupchat :: String        -- ^Nickname to use
              -> String       -- ^JID of room
              -> Maybe String -- ^Room password
              -> XMPP ()
joinGroupchat nick room password = do
    sendStanza $ XML "presence"
                   [("to",room++"/"++nick)]
                   [XML "x" [("xmlns","http://jabber.org/protocol/muc")]
                     passNode]
  where
    passNode = maybe [] (\pass -> [XML "password" [] [CData pass]]) password

-- |Leave groupchat.
leaveGroupchat :: String -> XMPP ()
leaveGroupchat room = sendStanza $ XML "presence"
                                     [("to",room),("type","unavailable")] []

-- |Return true if the stanza is a message of type \"groupchat\".
isGroupchatMessage :: StanzaPredicate
isGroupchatMessage = isMessage `conj` attributeMatches "type" (=="groupchat")

-- |Return true if the stanza is a private message in the named room.
isGroupchatPrivmsg :: String -> StanzaPredicate
isGroupchatPrivmsg room = matchesBare room `conj` attributeMatches "type" (=="chat")
                          `conj` attributeMatches "from" ((/="") . getResource)

-- |Send a groupchat message.
sendGroupchatMessage :: String  -- ^JID of chat room
                     -> String  -- ^Text of message
                     -> XMPP ()
sendGroupchatMessage room body =
    sendStanza $ XML "message"
                   [("to",room),
                    ("type","groupchat")]
                   [XML "body" [] [CData body]]

-- |Send a private message in a chat room.
sendGroupchatPrivateMessage :: String -- ^Nick of recipient
                            -> String -- ^JID of chat room
                            -> String -- ^Text of message
                            -> XMPP ()
sendGroupchatPrivateMessage nick room body =
    sendStanza $ XML "message"
                   [("to",room++"/"++nick),
                    ("type","chat")]
                   [XML "body" [] [CData body]]


getMessageSubject :: XMLElem -> Maybe String
getMessageSubject =
    cdata' . xmlPath ["subject"]

setGroupchatSubject :: String -- ^JID of chat room
                    -> String -- ^Subject
                    -> XMPP ()
setGroupchatSubject room subject =
    sendStanza $ XML "message"
                   [("to",room),("type","groupchat")]
                   [XML "subject" [] [CData subject]]


-- |Groupchat occupant.
data Occupant
  = Occupant
    { occRole :: Role
    , occAffiliation :: Affiliation
    , occNick :: String
    , occJid :: Maybe String
    , occStatus :: Status
    }

data Role = RModerator | RParticipant | RNone | RVisitor
          deriving (Eq, Show)
data Affiliation = AOwner | AAdmin | AMember | ANone | AOutcast
                 deriving (Eq, Show)

-- |Groupchat presence. Leave, Kick and Ban are role change too of
-- course, but it separated for simplicity sake.
data GroupchatPresence
    = Leave
    | Kick (Maybe String) -- ^Kick reason
    | Ban (Maybe String) -- ^Ban reason
    | NickChange String -- ^New nick
    | RoleChange (Maybe String) -- ^Role change (also show/status
                                -- change) with reason.

-- |Create groupchat presence from stanza.
doGroupchatPresence :: XMLElem -> (GroupchatPresence, Occupant)
doGroupchatPresence stanza =
    (presence, Occupant role aff nick jid status)
  where
    items = xmlPath' ["x", "item"] [stanza]
    item | length items == 0 = XML [] [] []
         | otherwise         = head items
    role = case getAttr "role" item of
      Just "moderator"   -> RModerator
      Just "participant" -> RParticipant
      Just "visitor"     -> RVisitor
      _                  -> RNone
    aff = case getAttr "affiliation" item of
      Just "owner"   -> AOwner
      Just "admin"   -> AAdmin
      Just "member"  -> AMember
      Just "outcast" -> AOutcast
      _              -> ANone
    presence = case getAttr "type" stanza of
      Just "unavailable" -> off_presence
      -- TODO: type=error?
      _                  -> RoleChange reason
    off_presence = case status_code of
      Just "301" -> Ban reason
      Just "303" -> NickChange new_nick
      Just "307" -> Kick reason
      -- TODO: parse more status codes?
      _          -> Leave
    reason = cdata' $ xmlPath ["reason"] item
    status = doStatus stanza
    nick = snd $ getJidRes stanza
    new_nick = maybe "" id $ getAttr "nick" item
    jid = getAttr "jid" item
    status_node = xmlPath' ["x", "status"] [stanza]
    status_code | length status_node == 0 = Nothing
                | otherwise               = getAttr "code"
                                            $ head status_node

-- |Handler for groupchat events (join/leave/kicks/bans/etc).
isGroupchatPresence :: StanzaPredicate
isGroupchatPresence stanza =
    (isPresence stanza) && (not $ null xs')
  where
    xs = xmlPath' ["x"] [stanza]
    xs' = filter
            (attributeMatches "xmlns" (=="http://jabber.org/protocol/muc#user"))
            xs


type Nick = String
type JID = String
-- |Do admin actions in groupchat.
adminGroupchat :: Either Nick JID -- ^Nickname or JID
               -> String -- ^JID of chat room
               -> String -- ^Role or affiliation argument
               -> (Maybe String) -- ^Reason
               -> XMPP ()
adminGroupchat nickOrJid room arg mReason =
    sendIq room "set"
               [XML "query"
                        [("xmlns","http://jabber.org/protocol/muc#admin")]
                        [item]]
    >> return ()
  where
    item = case nickOrJid of
             Left nick -> XML "item" [("nick",nick),("role",arg)] reason
             Right jid -> XML "item" [("jid",jid),("affiliation",arg)] reason
    reason = maybe [] (\r -> [XML "reason" [] [CData r]]) mReason
