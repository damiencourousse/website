{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Hakyll
import System.Process (readProcess)
import Text.Pandoc

import BibParse
import GHC.IO.Encoding


main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyll $ do

    -- static content
    mapM_ (`match` (route idRoute >> compile copyFileCompiler))
          [ "assets/js/*", "images/*", "media/*", "pdf/*", "CNAME" ]
    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Bibtex entries (for bibliography)
    match "assets/bib/*" $ compile biblioCompiler
    match "assets/csl/*" $ compile cslCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= relativizeUrls

    -- Static pages
    match ("pages/*.markdown" .||. "pages/*.md" .||. "pages/*.org") $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            pageCompiler
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx = constField "title" "miscellaneous notes"
                           <> listField "posts" context (loadAll "posts/*")
                           <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" builtPageCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- robots, etc.
    match "assets/txt/*" $ do
        route $ gsubRoute "assets/txt/" (const "")
        compile copyFileCompiler

    create ["atom.xml"] $ route idRoute >> compile (loadAllSnapshots "blog/*" "content" >>= renderAtom feedConfiguration context)
    create ["rss.xml"] $ route idRoute >> compile (loadAllSnapshots "blog/*" "content" >>= renderRss feedConfiguration context)

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let collectTags = return $ map (\(t,_) -> Item (tagsMakeId tags t) t) (tagsMap tags)
        sitemapCtx = tagsField "tags" tags
                   <> listField "alltags" context collectTags
                   <> builtPageCtx

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.html" sitemapCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
                { feedTitle = "Damien Couroussé"
                , feedDescription = ""
                , feedAuthorName = "Damien Couroussé"
                , feedAuthorEmail = "damien.courousse@gmail.com"
                , feedRoot = "http://damien.courousse.fr" }

-- Auxiliary compilers
--    the main page compiler
pageCompiler :: Compiler (Item String)
pageCompiler = do 
    bibFile <- getUnderlying >>= (flip getMetadataField "biblio")
    case bibFile of
         Just f -> bibtexCompiler f
         Nothing -> pandocCompiler

--    Biblio
bibtexCompiler :: String -> Compiler (Item String)
bibtexCompiler bibFile = do
    cslFile <- getUnderlying >>= (flip getMetadataField "csl")
    csl <- load (fromFilePath $ "assets/csl/" ++ (fromMaybe "chicago.csl" cslFile))
    bib <- load (fromFilePath $ "assets/bib/" ++ bibFile)
    liftM writePandoc
        (getResourceBody >>=
        preprocessBiblioCompiler bib >>=
        readPandocBiblio def csl bib)

preprocessBiblioCompiler :: Item Biblio            -- ^ the biblio references
                         -> Item String            -- ^ the page body
                         -> Compiler (Item String)
preprocessBiblioCompiler bib txt = return $ fmap (appendCitation bib) txt

appendCitation :: Item Biblio -> String -> String
appendCitation bib = processCitations refs
    where Biblio refs = itemBody bib

-- Context builders
builtPageCtx :: Context String
builtPageCtx = dateField "date" "%A, %e %B %Y"
        <> dateField "isodate" "%F"
        <> listField "pages" context (loadAll "pages/*")
        <> listField "posts" context (loadAll "posts/*")
        <> constField "siteroot" (feedRoot feedConfiguration)
        <> constField "git" "" -- create and empty "git" field required in the default template
        <> defaultContext

context :: Context String
context = teaserField "description" "content"
        <> gitTag "git"
        <> builtPageCtx

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
    <> dateField "dateArchive" "%b %e"
    <> defaultContext

-- | Extracts git commit info and render some html code for the page footer.
-- Copied and adapted from
-- Jorge.Israel.Peña at https://github.com/blaenk/blaenk.github.io
-- Miikka Koskinen at http://vapaus.org/text/hakyll-configuration.html
gitTag :: String -> Context String
gitTag key = field key $ \item -> do
  let fp = toFilePath $ itemIdentifier item
      gitLog format =
        readProcess "git" ["log", "-1", "HEAD", "--pretty=format:" ++ format, fp] ""
  unsafeCompiler $ do
    sha     <- gitLog "%h"
    date    <- gitLog "%aD"
    return $ "File last modified " ++ date ++ " (" ++ sha ++ ")"

