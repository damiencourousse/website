module BibParse
    (
      processCitations
    )
    where

import Control.Applicative ((<*))
import Data.List (intersperse)
import Text.Parsec
import Text.Parsec.String (Parser)

import Text.CSL hiding (processCitations)
import Text.CSL.Reference (Reference)
import Text.CSL.Input.Bibutils (readBiblioFile)
import Data.Maybe (fromMaybe)
import Text.CSL.Reference (emptyReference)
import Control.Monad (liftM, liftM2, mplus)
import Data.Maybe (fromJust)

main = test

test :: IO ()
test = do
  txt <- readFile "pages/publi.markdown"
  refs <- readBiblioFile "assets/bib/main.bib"
  putStrLn $ processCitations refs txt


data KeyData = KeyData { key :: Maybe String   -- the original key string
                       , keyId :: Maybe String -- the key id of the citation to process
                       , paper  :: Maybe String --
                       , slides :: Maybe String --
                       , poster :: Maybe String
                       , notes :: Maybe String
                       } deriving (Show)

emptyKeyData :: KeyData
emptyKeyData = KeyData { key = Nothing
                       , keyId = Nothing
                       , paper = Nothing
                       , slides = Nothing
                       , poster = Nothing
                       , notes = Nothing
                       }

processCitation :: [Reference] -- ^ the database of biblio references
                -> KeyData
                -> String      -- ^ the transformed citation
processCitation refs k = genCiteMetadata $ union [k, keydata]
    where keydata = case parse (parseBibtexNote <* eof) "" keyNote of
            Left err -> emptyKeyData { notes = Just $ show err }
            Right k  -> k
          keyNote = case href of
            Nothing -> "ERROR! reference not found!"
            Just h  -> renderPlain $ note h
          href = getReference refs citeKey
          citeKey = emptyCite { citeId = fromMaybe "malformed KeyData" $ keyId k }

genCiteMetadata :: KeyData -> String
genCiteMetadata k  = unwords [ fromMaybe "key: empty" $ key k
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

                             -- TODO  how to generate links for other formats?
                             , genPaper $ paper k
                             , genSlides $ slides k
                             , genPoster $ poster k
                             ]

genPaper :: Maybe String -> String
genPaper (Just k) = "[Pdf](" ++ k ++ ") "
genPaper Nothing  = ""

genSlides :: Maybe String -> String
genSlides (Just k) = "[Slides](" ++ k ++ ") "
genSlides Nothing  = ""

genPoster :: Maybe String -> String
genPoster (Just k) = "[Poster](" ++ k ++ ") "
genPoster Nothing  = ""



parseBibtexNote :: Parser KeyData
parseBibtexNote = do
    elems <- parseNoteElement `sepBy` (char ',')
    return $ union elems

-- | concatenates all the Just fields found. In case of collision, the latest element has precedence
-- FIXME à tester
union :: [KeyData] -> KeyData
union ks = foldr step emptyKeyData ks
  where
    step :: KeyData -> KeyData -> KeyData
    step (KeyData a b c d e f) (KeyData a' b' c' d' e' f') =
        KeyData (mplus a a')
                (mplus b b')
                (mplus c c')
                (mplus d d')
                (mplus e e')
                (mplus f f')

parseNoteElement :: Parser KeyData
parseNoteElement = try parsePaper
                 <|> try parseSlides
                 <|> try parsePoster
                 <|> try parseOtherNotes

parseElem :: String             -- ^ the element identifier
          -> Parser String
parseElem e = do
    spaces
    _ <- string e <* spaces
    _ <- string "=" <* spaces
    many1 $ noneOf ","

-- |
-- >>> parse parsePaper  "" "paper=/media/paper_0.pdf"
-- Right (KeyData {key = Nothing, keyId = Nothing, paper = Just "/media/paper_0.pdf", slides = Nothing, poster = Nothing, notes = Nothing})
parsePaper :: Parser KeyData
parsePaper = do
    liftM (\x -> emptyKeyData { paper = Just x }) $
        parseElem "paper"

parseSlides :: Parser KeyData
parseSlides = do
    liftM (\x -> emptyKeyData { slides = Just x }) $
        parseElem "slides"

parsePoster :: Parser KeyData
parsePoster = do
    liftM (\x -> emptyKeyData { poster = Just x }) $
        parseElem "poster"

parseOtherNotes :: Parser KeyData
parseOtherNotes = do
    liftM (\x -> emptyKeyData { notes = Just x }) $
        many1 $ noneOf ","


parse' :: Parser String -> String -> String
parse' p txt = fromEither $ parse (p <* eof) "" txt

fromEither :: Either ParseError String -> String
fromEither e = case e of
    Left err -> show err
    Right c -> c


processCitations :: [Reference] -> String -> String
processCitations refs body = parse' (parseCitations $ processCitation refs) body

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
    string "[@"
    k <- many1 $ choice [alphaNum, char ':']
    string "]"
    return $ p (emptyKeyData { key = Just $ "[@"++k++"]", keyId = Just k })

parseInTextCite :: (KeyData -> String) -> Parser String
parseInTextCite p = do
    string "@"
    k <- many1 $ choice [alphaNum, char ':']
    return $ p (emptyKeyData { key = Just $ "@"++k, keyId = Just k })

