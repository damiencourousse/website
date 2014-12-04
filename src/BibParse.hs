module BibParse
    ( processCitations )
    where

import Control.Applicative ((<*))
import Control.Monad (liftM, liftM2, mplus)
import Data.Maybe (fromMaybe)
import Data.String.Utils (strip)
import Text.CSL hiding (processCitations)
import Text.Parsec
import Text.Parsec.String (Parser)

data KeyData = KeyData { key :: Maybe String   -- the original key string
                       , keyId :: Maybe String -- the key id of the citation to process
                       , notes :: Maybe String
                       , links :: [(Ltext, Url)]
                       } deriving (Show)

newtype Ltext = Ltext String  deriving (Show)
newtype Url = Url String  deriving (Show)

test :: IO ()
test = do
  txt <- readFile "pages/publi.markdown"
  refs <- readBiblioFile "assets/bib/main.bib"
  putStrLn $ processCitations refs txt

emptyKeyData :: KeyData
emptyKeyData = KeyData { key = Nothing
                       , keyId = Nothing
                       , notes = Nothing
                       , links = []
                       }

processCitation :: [Reference] -- ^ the database of biblio references
                -> KeyData
                -> String      -- ^ the transformed citation
processCitation refs k = genCiteMetadata $ union [k, keydata]
    where keydata = case parse (parseBibtexNote <* eof) "" keyNote of
            Left err -> emptyKeyData { notes = Just $ show err }
            Right k' -> k'
          keyNote = case href of
            Nothing -> "ERROR! reference not found!"
            Just h  -> renderPlain $ note h
          href = getReference refs citeKey
          citeKey = emptyCite { citeId = fromMaybe "malformed KeyData" $ keyId k }

genCiteMetadata :: KeyData -> String
genCiteMetadata k  = unwords $ [ fromMaybe "key: empty" $ key k
                             {-, fromMaybe "keyId: empty\n" $ keyId k-}
                             --
                             -- Markdown links always start with the link text
                             -- enclosed in brackets. This HTML makes a clear
                             -- separation with the previous biblio citation.
                             -- It avoids the first link text in brackets to be
                             -- interpreted as a followup of the citation, such
                             -- as in this example:
                             -- @smith04 [p. 33] says blah.
                             -- refs: http://johnmacfarlane.net/pandoc/README.html#citations
                             , "<!-- here follow the markdown metadata -->"
                             ]
                             ++ (map genHrefs $ links k)

genHrefs :: (Ltext, Url) -> String
genHrefs (Ltext h, Url u) = "[" ++ h ++ "](" ++ u ++ ") "


-- |
-- >>> parse parseBibtexNote   "" "paper=/media/paper_0.pdf, poster = /poster.pdf"
-- Right (KeyData {key = Nothing, keyId = Nothing, notes = Nothing, links = [(Ltext "paper",Url "/media/paper_0.pdf"),(Ltext "poster",Url "/poster.pdf")]})
parseBibtexNote :: Parser KeyData
parseBibtexNote = do
    elems <- parseNoteElement `sepBy` char ','
    return $ union elems

-- | concatenates all the Just fields found. In case of collision, the latest element has precedence
-- FIXME à tester
union :: [KeyData] -> KeyData
union = foldr step emptyKeyData
  where
    step :: KeyData -> KeyData -> KeyData
    step (KeyData a b n xs) (KeyData a' b' n' xs') =
        KeyData (mplus a a')
                (mplus b b')
                (mplus n n')
                (xs ++ xs')

parseNoteElement :: Parser KeyData
parseNoteElement =   try parseLink
                 <|> try parseNotes

-- | extract a link from a note field
--
-- >>> parse parseLink  "" "paper=/media/paper_0.pdf"
-- Right (KeyData {key = Nothing, keyId = Nothing, notes = Nothing, links = [(Ltext "paper",Url "/media/paper_0.pdf")]})
--
-- >>> parse parseLink  "" " paper=/media/paper_0.pdf "
-- Right (KeyData {key = Nothing, keyId = Nothing, notes = Nothing, links = [(Ltext "paper",Url "/media/paper_0.pdf")]})
--
-- >>> parse parseLink  "" " link name with spaces =/media/paper_0.pdf "
-- Right (KeyData {key = Nothing, keyId = Nothing, notes = Nothing, links = [(Ltext "link name with spaces",Url "/media/paper_0.pdf")]})
--
-- >>> parse parseLink  "" "a few notes, link=http://url.com"
-- Right (KeyData {key = Nothing, keyId = Nothing, notes = Nothing, links = [(Ltext "afewnotes,link",Url "http://url.com")]})
--
-- >>> parse parseLink  "" "link=http://url.com, a few notes"
-- Right (KeyData {key = Nothing, keyId = Nothing, notes = Nothing, links = [(Ltext "link",Url "http://url.com")]})
parseLink :: Parser KeyData
parseLink  = do
    spaces
    href <- many1 $ noneOf "="
    _ <- string "=" <* spaces
    u <- many1 $ noneOf ","
    return $ emptyKeyData { links = [(Ltext $ strip href, Url $ strip u)] }

-- | extract a sequence of characters terminated by a comma from a note field
--
-- >>> parse parseNotes  "" "a few notes"
-- Right (KeyData {key = Nothing, keyId = Nothing, notes = Just "a few notes", links = []})
--
-- >>> parse parseNotes  "" "a few notes, link=http://url.com"
-- Right (KeyData {key = Nothing, keyId = Nothing, notes = Just "a few notes", links = []})
parseNotes :: Parser KeyData
parseNotes = liftM (\x -> emptyKeyData { notes = Just x }) $ many1 $ noneOf ","

parseStr :: Parser String -> String -> String
parseStr p txt = fromEither $ parse (p <* eof) "" txt

fromEither :: Either ParseError String -> String
fromEither e = case e of
    Left err -> show err
    Right c -> c


processCitations :: [Reference] -> String -> String
processCitations refs = parseStr (parseCitations $ processCitation refs)

-- |
-- parse a page body, extract bibliographic citations,
-- and apply the text transformer to each bibliographic citation found.
--
-- FIXME the text transformer p should be carried by the Parser monad
--
parseCitations :: (KeyData -> String) -> Parser String
parseCitations p = do
    elems <- many1 $ parsePageElements p
    return $ concat elems

parsePageElements :: (KeyData -> String) -> Parser String
parsePageElements p = try (parseCite p)
                    <|> try (parseInTextCite p)
                    <|> try (liftM2 (:) (noneOf "@") (many $ noneOf "[@"))

parseCite :: (KeyData -> String) -> Parser String
parseCite p = do
    _ <- string "[@"
    k <- many1 $ choice [alphaNum, char ':']
    _ <- string "]"
    return $ p (emptyKeyData { key = Just $ "[@"++k++"]", keyId = Just k })

parseInTextCite :: (KeyData -> String) -> Parser String
parseInTextCite p = do
    _ <- string "@"
    k <- many1 $ choice [alphaNum, char ':']
    return $ p (emptyKeyData { key = Just $ "@"++k, keyId = Just k })

