{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import           Hakyll
import           System.Process  (readProcess)
import           Text.Pandoc

import           BibParse
import           GHC.IO.Encoding


main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyll $ do

    -- static content
    mapM_ (`match` (route idRoute >> compile copyFileCompiler))
          [ "assets/js/*"
          , "images/*"
          , "media/*"
          , "pdf/*"
          , "CNAME" ]
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
    match ("pages/publications.markdown") $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $
            publiCompiler
                >>= loadAndApplyTemplate "templates/default.html" builtPageCtx
                >>= relativizeUrls
    match ("pages/*.markdown" .||. "pages/*.md" .||. "pages/*.org") $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $
            pandocIOCompiler
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

    -- robots, sitemap, etc.
    match "assets/txt/*" $ do
        route $ gsubRoute "assets/txt/" (const "")
        compile copyFileCompiler

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
                  , feedAuthorEmail = "no email"
                  , feedRoot = "http://damien.courousse.fr"
                  }

-- Auxiliary compilers
--    the main page compiler
publiCompiler :: Compiler (Item String)
publiCompiler = do
    bibFile <- getUnderlying >>= \i -> getMetadataField i "biblio"
    case bibFile of
         Just f  -> bibtexCompiler f
         Nothing -> pandocCompiler

--    with the full pandoc instead of 'PandocPure'
pandocIOCompiler :: Compiler (Item String)
pandocIOCompiler = writePandoc <$> (getResourceBody >>= readPandocIOWith def)

-- Hakyll's version of 'readPandoc', but replacing the pure instance
-- of 'PandocMonad' with the one running in 'IO'.  'PandocPure' does
-- not support all pandoc functionalities, in particular, org
-- includes.
-- https://github.com/jaspervdj/hakyll/issues/635
readPandocIOWith
  :: ReaderOptions -- ^ Parser options
  -> Item String -- ^ String to read
  -> Compiler (Item Pandoc) -- ^ Resulting document
readPandocIOWith ropt item =
  unsafeCompiler $
  runIO (traverse (reader ropt (itemFileType item)) (fmap T.pack item)) >>=
  \x -> case x of
    Left err ->
      fail $ "Hakyll.Web.Pandoc.readPandocWith: parse failed: " ++ show err
    Right item' -> return item'
  where
    reader ro t =
      case t of
        DocBook -> readDocBook ro
        Html -> readHtml ro
        LaTeX -> readLaTeX ro
        LiterateHaskell t' -> reader (addExt ro Ext_literate_haskell) t'
        Markdown -> readMarkdown ro
        MediaWiki -> readMediaWiki ro
        OrgMode -> readOrg ro
        Rst -> readRST ro
        Textile -> readTextile ro
        _ ->
          error $
          "Hakyll.Web.readPandocWith: I don't know how to read a file of " ++
          "the type " ++ show t ++ " for: " ++ show (itemIdentifier item)
    addExt ro e =
      ro {readerExtensions = enableExtension e $ readerExtensions ro}

--    Biblio
bibtexCompiler :: String -> Compiler (Item String)
bibtexCompiler bibFile = do
    cslFile <- getUnderlying >>= \i -> getMetadataField i "csl"
    csl <- load (fromFilePath $ "assets/csl/" ++ fromMaybe "chicago.csl" cslFile)
    bib <- load (fromFilePath $ "assets/bib/" ++ bibFile)
    getResourceBody
        >>= appendCitations bib
        >>= readPandocBiblio
            (def {readerExtensions = pandocExtensions })
            csl bib
        >>= \x -> return $ writePandoc x

appendCitations :: Item Biblio -> Item String -> Compiler (Item String)
appendCitations bib t = pure (processCitations refs <$> t)
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
--
-- Adapted from
-- - Jorge.Israel.Peña at https://github.com/blaenk/blaenk.github.io
-- - Miikka Koskinen at http://vapaus.org/text/hakyll-configuration.html
gitTag :: Context String
gitTag = field "gitinfo" $ \item -> do
  let fp = toFilePath $ itemIdentifier item
      gitLog format =
        readProcess "git" ["log", "-1", "HEAD", "--pretty=format:" ++ format, fp] ""
  unsafeCompiler $ do
    date    <- gitLog "%aD"
    return $ concat
             [ "<a href=https://github.com/damiencourousse/damiencourousse.github.io/commits/master>"
             , "Page last modified " ++ date
             , "</a>"
             ]

-- | Extract the last modification date from the git commits
lastGitModification :: Context a
lastGitModification = field "lastgitmod" $ \item -> do
  let fp = toFilePath $ itemIdentifier item
  unsafeCompiler $
    readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%ad", "--date=short", fp] ""
