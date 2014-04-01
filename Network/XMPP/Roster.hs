module Network.XMPP.Roster (
    RosterItem(..),
    Subscription(..),
    getRoster,
    Presence(..),
    Status(..),
    StatusType(..),
    doPresence,
    doStatus
) where

import Network.XMPP.XMPPMonad
import Network.XMPP.XMLParse
import Network.XMPP.Stanzas

-- TODO: add/delete roster items (via sending IQs)
--       add/delete subscriptions


data RosterItem = RosterItem
    { itemName :: String
    , itemJid :: String
    , itemSubscription :: Subscription
    , itemGroups :: [String]
    } deriving Show

-- "There are nine possible subscription states"
-- It's horrible.
data Subscription = SBoth | SFrom | STo | SNone | SUnknown
                    deriving Show


getRoster :: XMPP [RosterItem]
getRoster = do
    stanza <- sendIqWait "" "get" [XML "query" [("xmlns", "jabber:iq:roster")] []]
    let items = xmlPath' ["query", "item"] [stanza]
    return $ map getItem items
  where
    getItem item =
      let getAttr' attr = maybe "" id $ getAttr attr item

          name = getAttr' "name"
          jid = getAttr' "jid"
          groups = map cdata $ xmlPath' ["group"] [item]
          subs = case getAttr' "subscription" of
                   "to" -> STo
                   "from" -> SFrom
                   "both" -> SBoth
                   "none" -> SNone
                   _ -> SUnknown
      in RosterItem name jid subs groups


--- Presences

data Presence = Available Status
              | Unavailable Status
              | Subscribe
              | Subscribed
              | Unsubscribe
              | Unsubscribed
              | Probe
              | Error

-- | TODO: xml:lang for multiple statuses.
data Status = Status StatusType [String]

data StatusType = StatusOnline
                | StatusAway
                | StatusChat
                | StatusDND
                | StatusXA
                | StatusOffline


-- | Read presence stanza.
doPresence :: XMLElem -> Presence
doPresence stanza =
    let stanzaType = getAttr "type" stanza
        status@(Status _ statuses) = doStatus stanza
    in case stanzaType of
         Nothing -> Available status
         Just "unavailable" -> Unavailable (Status StatusOffline statuses)
         Just "subscribe" -> Subscribe
         Just "subscribed" -> Subscribed
         Just "unsubscribe" -> Unsubscribe
         Just "unsubscribed" -> Unsubscribed
         Just "probe" -> Probe
         Just "error" -> Error
         _ -> Error

-- | Read stanza status.
doStatus :: XMLElem -> Status
doStatus stanza = Status statusType statuses
  where
    statuses = map cdata $ xmlPath' ["status"] [stanza]
    statusType = case cdata' $ xmlPath ["show"] stanza of
                   Just "away" -> StatusAway
                   Just "chat" -> StatusChat
                   Just "dnd" -> StatusDND
                   Just "xa" -> StatusXA
                   _ -> StatusOnline
