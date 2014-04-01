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
    , getRest
    , xmppStreamStart
    , shallowTag
    , deepTag
    , deepTags
    , Text.ParserCombinators.Parsec.parse
    , Text.ParserCombinators.Parsec.Parser
    , xmlPath'
    , allChilds
    )
    where

import Text.ParserCombinators.Parsec
import List

-- |A data structure representing an XML element.
data XMLElem = XML String [(String,String)] [XMLElem]
             -- ^Tags have a name, a list of attributes, and a list of
             -- child elements.
             | CData String
               -- ^Character data just contains a string.
               deriving (Show, Eq)

-- |Follow a \"path\" of named subtags in an XML tree.  For every
-- element in the given list, find the subtag with that name and
-- proceed recursively.
xmlPath :: [String] -> XMLElem -> Maybe XMLElem
xmlPath [] el = return el
xmlPath (name:names) (XML _ _ els) =
    do
      el <- find (\stanza ->
                      case stanza of
                        (XML n _ _) -> name==n
                        _ -> False) els
      xmlPath names el

-- |Get the value of an attribute in the given tag.
getAttr :: String -> XMLElem -> Maybe String
getAttr attr (XML _ attrs _) = lookup attr attrs

-- |Get the character data subelement of the given tag.
getCdata :: XMLElem -> Maybe String
getCdata (XML _ _ els) =
    case els of
      [CData s] -> Just s
      _ -> Nothing

-- |Convert the tag back to XML.  If the first parameter is true,
-- close the tag.
xmlToString :: Bool -> XMLElem -> String
xmlToString _ (CData s) = replaceToEntities s
xmlToString close (XML name attrs subels) =
    "<" ++ name ++ attrsToString attrs ++
        if close then
            ">" ++ (concat $ map (xmlToString True) subels)
                ++ "</" ++ name ++ ">"
            else
                ">"

-----------------------------------------------
-- |Replace special characters to XML entities.
replaceToEntities :: String -> String
replaceToEntities [] = []
replaceToEntities (char:chars) =
    let str = case char of
                '&' -> "&amp;"
                '<' -> "&lt;"
                '>' -> "&gt;"
                '"' -> "&quot;"
                '\'' -> "&apos;"
                _ -> char:[]
    in str ++ (replaceToEntities chars)
-----------------------------------------------

attrsToString :: [(String,String)] -> String
attrsToString [] = ""
attrsToString ((name,value):attrs) =
    " "++name++"='"++value++"'" ++ attrsToString attrs

getRest :: Parser a -> Parser (a, String)
getRest f = do x <- try f
               p <- getInput
               return (x, p)

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
      return $ XML name attrs []

attribute :: Parser (String, String)
attribute =
    do
      name <- many1 tokenChar
      char '='
      quote <- char '\'' <|> char '"'
      value <- many $ satisfy (/=quote)
      char quote
      return (name, value)

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
      return $ CData text
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
-----------------------------------------------

tokenChar :: Parser Char
tokenChar = letter <|> char ':' <|> char '-'

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
xmlPath' :: [String] -> [XMLElem] -> [XMLElem]
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
    in xmlPath' names (concat elems')

-----------------------------------------------
-- |Get all childs of the XML element
allChilds :: XMLElem -> [XMLElem]
allChilds (XML _ _ c) = c
