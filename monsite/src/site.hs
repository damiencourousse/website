--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Control.Monad (liftM)
import Data.Monoid (mappend, mconcat)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), liftA2)
import Hakyll.Core.Compiler (getResourceString)
import Text.Pandoc.Options

import BibParse

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "media/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy static assets
    match "assets/js/*" $ do
        route   idRoute
        compile copyFileCompiler

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
                    constField "title" "posts archives"      `mappend`
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


-- Auxiliary compilers
pageCompiler :: Item String -> Compiler (Item String)
pageCompiler i = loadAndApplyTemplate "templates/default.html" defaultContext i
               >>= relativizeUrls

bibtexCompiler :: String -> String -> Compiler (Item String)
bibtexCompiler cslFileName bibFileName = do 
    -- TODO System.FilePath.Posix (</>) :: FilePath -> FilePath -> FilePath
    csl <- load (fromFilePath $ "assets/csl/" ++ cslFileName)
    bib <- load (fromFilePath $ "assets/bib/" ++ bibFileName)
    liftM writePandoc
        (getResourceBody >>=
         preprocessBiblioCompiler bib >>=
         readPandocBiblio def csl bib)

preprocessBiblioCompiler :: Item Biblio            -- ^ the biblio references
                         -> Item String            -- ^ the page body
                         -> Compiler (Item String)
preprocessBiblioCompiler bib txt = return $ fmap (truc bib) txt

truc :: Item Biblio -> String -> String
-- TODO utiliser truc:: Biblio -> String -> String
truc bib = processCitations refs
    where Biblio refs = itemBody bib

--------------------------------------------------------------------------------
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


{-writerConfig = def-}
{-readerConfig = def { readerSmart = True, readerOldDashes = True }-}
{-myPandocCompiler = pandocCompilerWith readerConfig writerConfig-}
