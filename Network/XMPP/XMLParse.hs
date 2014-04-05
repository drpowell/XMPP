{-# LANGUAGE OverloadedStrings #-}

-- |The difference between this XML parsers and all other XML parsers
-- is that this one can parse an XML document that is only partially
-- received, returning the parts that have arrived so far.
module Network.XMPP.XMLParse
    ( XMLElem(..)
    , xmlPath
    , getAttr
    , getCdata
    , xmlToString
    , attrsToString
    , xmppStreamStart
    , shallowTag
    , deepTag
    , deepTags
    , Data.Attoparsec.Text.parse
    , Data.Attoparsec.Text.Parser
    , Data.Attoparsec.Text.IResult(..)
    , xmlPath'
    , allChilds
    )
    where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative ((<|>))
import Data.List

-- |A data structure representing an XML element.
data XMLElem = XML Text [(Text,Text)] [XMLElem]
             -- ^Tags have a name, a list of attributes, and a list of
             -- child elements.
             | CData Text
               -- ^Character data just contains a string.
               deriving (Show, Eq)

-- |Follow a \"path\" of named subtags in an XML tree.  For every
-- element in the given list, find the subtag with that name and
-- proceed recursively.
xmlPath :: [Text] -> XMLElem -> Maybe XMLElem
xmlPath [] el = return el
xmlPath (name:names) (XML _ _ els) =
    do
      el <- find (\stanza ->
                      case stanza of
                        (XML n _ _) -> name==n
                        _ -> False) els
      xmlPath names el
xmlPath _ _ = error "Unexpected use of xmlPath"

-- |Get the value of an attribute in the given tag.
getAttr :: Text -> XMLElem -> Maybe Text
getAttr attr (XML _ attrs _) = lookup attr attrs
getAttr _ _ = error "Unexpected use of getAttr"

-- |Get the character data subelement of the given tag.
getCdata :: XMLElem -> Maybe Text
getCdata (XML _ _ els) =
    case els of
      [CData s] -> Just s
      _ -> Nothing
getCdata _ = error "Unexpected use of getCdata"

-- |Convert the tag back to XML.  If the first parameter is true,
-- close the tag.
xmlToString :: Bool -> XMLElem -> Text
xmlToString _ (CData s) = replaceToEntities s
xmlToString close (XML name attrs subels) =
    T.concat $ ["<", name, attrsToString attrs,">"] ++ sub
  where
    sub | close = [T.concat $ map (xmlToString True) subels, "</", name, ">"]
        | otherwise = []

-----------------------------------------------
-- |Replace special characters to XML entities.
replaceToEntities :: Text -> Text
replaceToEntities str = T.concatMap repl str
  where
    repl c = case c of
                '&' -> "&amp;"
                '<' -> "&lt;"
                '>' -> "&gt;"
                '"' -> "&quot;"
                '\'' -> "&apos;"
                c -> T.singleton c
-----------------------------------------------

attrsToString :: [(Text,Text)] -> Text
attrsToString [] = ""
attrsToString ((name,value):attrs) =
    T.concat [" ", name, "='", value, "'", attrsToString attrs]

many = many'

xmppStreamStart :: Parser XMLElem
xmppStreamStart =
    do
      many $ try processingInstruction
      streamTag <- shallowTag
      return streamTag

shallowTag :: Parser XMLElem
shallowTag =
    do
      tag <- tagStart
      char '>'
      return tag

deepTags :: Parser [XMLElem]
deepTags = many $ try deepTag

deepTag :: Parser XMLElem
deepTag =
    do
      (XML name attrs _) <- tagStart
      subels <-
          (try $ do
             char '/'
             char '>'
             return [])
          <|>
          do
            char '>'
            els <- many $ (try deepTag) <|> cdata
            char '<'
            char '/'
            string name
            char '>'
            return els
      return $ XML name attrs subels

tagStart :: Parser XMLElem
tagStart =
    do
      char '<'
      name <- many1 tokenChar
      many space
      attrs <- many $
               do
                 attr <- attribute
                 many space
                 return attr
      return $ XML (T.pack name) attrs []

attribute :: Parser (Text, Text)
attribute =
    do
      name <- many1 tokenChar
      char '='
      quote <- char '\'' <|> char '"'
      value <- many $ satisfy (/=quote)
      char quote
      return (T.pack name, T.pack value)

-----------------------------------------------
-- cdata :: Parser XMLElem
-- cdata =
--    do
--      text <- many1 plainCdata
--      return $ CData text
--    where plainCdata = satisfy (\c -> c/='<')

cdata :: Parser XMLElem
cdata =
    do
      text <- many1 $ plainCdata <|> predefinedEntity
      return $ CData $ T.pack text
    where plainCdata = satisfy (\c -> c/='<' && c/='&')
          predefinedEntity = do
            char '&'
            entity <- try (string "amp")
                      <|> try (string "lt")
                     <|> try (string "gt")
                     <|> try (string "quot")
                     <|> string "apos"
            char ';'
            return $ case entity of
                       "amp" -> '&'
                       "lt" -> '<'
                       "gt" -> '>'
                       "quot" -> '"'
                       "apos" -> '\''
                       x -> error $ "Unknown entity in cdata : "++show x
-----------------------------------------------

tokenChar :: Parser Char
tokenChar = letter <|> char ':' <|> char '-' <|> char '_' <|> char '.'

processingInstruction :: Parser ()
processingInstruction =
    do
      char '<'
      char '?'
      many $ satisfy (/='?')
      char '?'
      char '>'
      return ()

-----------------------------------------------
xmlPath' :: [Text] -> [XMLElem] -> [XMLElem]
xmlPath' [] [] = []
xmlPath' [] els = els
xmlPath' (name:names) elems =
    let elems' = map filter_elem elems
        filter_elem (XML _ _ els) =
            filter (\stanza ->
                     case stanza of
                       (XML n _ _) -> name==n
                       _ -> False
                   ) els
        filter_elem _ = error "Unexpected use of filter_elem"
    in xmlPath' names (concat elems')
-----------------------------------------------
-- |Get all childs of the XML element
allChilds :: XMLElem -> [XMLElem]
allChilds (XML _ _ c) = c
allChilds _ = error "Unexpected use of allChilds"
