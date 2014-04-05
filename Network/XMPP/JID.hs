{-# LANGUAGE OverloadedStrings #-}
module Network.XMPP.JID where

-- TODO: do type alias for jid?
-- type JID = String

import Data.Text (Text)
import qualified Data.Text as T

-- |Get username part of JID, i.e. the part before the \@ sign.
-- Return @\"\"@ if the JID contains no \@ sign.
getUsername :: Text -> Text
getUsername jid =
    case T.break (=='@') jid of
      (username, rest) | not (T.null rest) -> username
      _ -> ""

-- |Get resource part of JID, i.e. the part after \/.
-- Return @\"\"@ if the JID has no resource.
getResource :: Text -> Text
getResource jid = T.drop 1 $ T.dropWhile (/='/') jid

-- |Get the bare JID, i.e. everything except the resource.
getBareJid :: Text -> Text
getBareJid jid = T.takeWhile (/='/') jid
