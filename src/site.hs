{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mappend, mconcat)
import Hakyll
import Text.Pandoc

import BibParse
import GHC.IO.Encoding


main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyll $ do

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    let collectTags = return $ map (\(t,_) -> Item (tagsMakeId tags t) t) (tagsMap tags)
        ctx = tagsField "tags" tags <> listField "alltags" context collectTags <> context
        template t = loadAndApplyTemplate (fromFilePath $ "templates/" ++ t ++ ".html") ctx

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
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- Static pages
    match ("pages/*.markdown" .||. "pages/*.md" .||. "pages/*.org") $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            item <- getUnderlying
            bibFile <- liftM (fromMaybe "") $ getMetadataField item "biblio"
            cslFile <- liftM (fromMaybe "chicago.csl") $ getMetadataField item "csl"
            let compiler = if bibFile /= "" then
                                bibtexCompiler cslFile bibFile
                           else pandocCompiler
            compiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "miscellaneous notes"      `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- robots, etc.
    match "assets/txt/*" $ do
        route $ gsubRoute "assets/txt/" (const "")
        compile copyFileCompiler

    create ["atom.xml"] $ route idRoute >> compile (loadAllSnapshots "blog/*" "content" >>= renderAtom feedConfiguration context)
    create ["rss.xml"] $ route idRoute >> compile (loadAllSnapshots "blog/*" "content" >>= renderRss feedConfiguration context)
    create ["sitemap.xml"] $ route idRoute >> compile (makeItem "" >>= template "sitemap")

    match "templates/*" $ compile templateCompiler


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
                { feedTitle = "Damien Couroussé"
                , feedDescription = ""
                , feedAuthorName = "Damien Couroussé"
                , feedAuthorEmail = "damien.courousse@gmail.com"
                , feedRoot = "http://damien.courousse.fr" }

context :: Context String
context = dateField "date" "%A, %e %B %Y"
        <> dateField "isodate" "%F"
        <> listField "pages" context (loadAll "pages/*")
        <> listField "posts" context (loadAll "posts/*")
        -- <> listField "pics" context (loadAll $ "static/pics/*.png" .&&. hasVersion "map")
        <> constField "siteroot" (feedRoot feedConfiguration)
        <> teaserField "description" "content"
        <> defaultContext



-- Auxiliary compilers
bibtexCompiler :: String -> String -> Compiler (Item String)
bibtexCompiler cslFileName bibFileName = do 
    csl <- load (fromFilePath $ "assets/csl/" ++ cslFileName)
    bib <- load (fromFilePath $ "assets/bib/" ++ bibFileName)
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

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/archive-item.html"
    posts <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList postItemTpl (tagsCtx tags) posts

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat [ tagsField "prettytags" tags
                       , postCtx
                       ]

