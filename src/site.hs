{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Hakyll
import System.Process (readProcess, readCreateProcess, shell)
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
            >>= loadAndApplyTemplate "templates/default.html" builtPageCtx
            >>= relativizeUrls

    -- Static pages
    match ("pages/*.markdown" .||. "pages/*.md" .||. "pages/*.org") $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            pageCompiler
                >>= loadAndApplyTemplate "templates/default.html" builtPageCtx
                >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =  constField "title" "miscellaneous notes"
                           <> listField "posts" builtPageCtx (loadAll "posts/*")
                           <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- robots, etc.
    match "assets/txt/*" $ do
        route $ gsubRoute "assets/txt/" (const "")
        compile copyFileCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            -- posts <- recentFirst =<< loadAll "posts/*"
            -- pages <- loadAll "pages/*"
            -- let allPages = pages ++ posts
            let sitemapCtx = builtPageCtx <> lastGitModification
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
                  { feedTitle = "Damien Couroussé"
                  , feedDescription = ""
                  , feedAuthorName = "Damien Couroussé"
                  , feedAuthorEmail = "damien.courousse@gmail.com"
                  , feedRoot = "http://damien.courousse.fr"
                  }

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
builtPageCtx =  constField "siteroot" (feedRoot feedConfiguration)
             <> listField "entries" builtPageCtx (loadAll $ "pages/*" .||. "posts/*")
             <> dateField "date" "%A, %e %B %Y"
             <> dateField "isodate" "%F"
             <> gitTag
             <> lastGitModification
             <> defaultContext

postCtx :: Context String
postCtx =  dateField "date" "%B %e, %Y"
        <> dateField "dateArchive" "%b %e"
        <> modificationTimeField "mtime" "%F"
        <> defaultContext

-- | Extracts git commit info and render some html code for the page footer.
-- Adapted from
-- Jorge.Israel.Peña at https://github.com/blaenk/blaenk.github.io
-- Miikka Koskinen at http://vapaus.org/text/hakyll-configuration.html
gitTag :: Context String
gitTag = field "gitinfo" $ \item -> do
  let fp = toFilePath $ itemIdentifier item
      gitLog format =
        readProcess "git" ["log", "-1", "HEAD", "--pretty=format:" ++ format, fp] ""
  route <- fromMaybe "" <$> (getRoute =<< getUnderlying)
  unsafeCompiler $ do
    sha     <- gitLog "%h"
    date    <- gitLog "%aD"

    -- the target file is build in a separate git repository, located in site/.
    -- We need to invocate git from this subdirectory to retrieve the last
    -- modification commit of this built file.
    routesha <- readCreateProcess (shell $ "cd site && git log -1 HEAD --pretty=format:%H " ++ route) ""

    let shaurl = concat
               [ "<a href=https://github.com/damiencourousse/damiencourousse.github.io/commit/"
               , routesha
               , ">"
               , sha
               , "</a>"
               ]
    return $ "File last modified " ++ date ++ " (" ++ shaurl ++ ")"

-- | Extract the last modification date from the git commits
lastGitModification :: Context a
lastGitModification = field "lastgitmod" $ \item -> do
  let fp = toFilePath $ itemIdentifier item
  unsafeCompiler $
    readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%ad", "--date=short", fp] ""

