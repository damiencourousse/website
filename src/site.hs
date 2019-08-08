{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative           (Alternative (..))
import           Control.Monad                 (liftM)
import           Data.List                     (sortBy)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import           Data.Ord                      (comparing)
import qualified Data.Text                     as T
import           Hakyll
import           Hakyll.Core.Compiler.Internal
import           System.Directory              (createDirectoryIfMissing,
                                                doesFileExist)
import           System.FilePath
import qualified System.Process                as Process
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
    match ( "assets/js/*" .||. "images/*" .||. "media/*" .||. "pdf/*" ) $ do
      route idRoute
      compile copyFileCompiler
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


    -- the photo gallery -------------------------

    -- copy original images
    let expUImages :: Pattern
        expUImages =      "images/expressions-urbaines/*.jepg"
                     .||. "images/expressions-urbaines/*.jpg"
                     .||. "images/expressions-urbaines/*.JPG"
                     .||. "images/expressions-urbaines/*.png"

    match expUImages $ do
      route $ gsubRoute "expressions-urbaines/" (const "")
      compile copyFileCompiler

    -- create thumbnails
    match expUImages $ version "small" $ do
      route $
        gsubRoute "expressions-urbaines/" (const "") `composeRoutes` appendFileRoute "small"
      compile $ resizeImage 600 600

    -- the gallery page
    create ["expressions-urbaines.html"] $ do
      route idRoute
      compile $ do
        let imageCtx :: Context a
            imageCtx = urlField "url"
                       <> basenameField "id"
                       <> pathField "path"
                       <> mdateFile "mdate"
                       <> imageAlt "alt"

            photosCtx :: Context String
            photosCtx = listField "photos" (imageCtx :: Context CopyFile)
                        (reverse <$> (loadAll ("images/expressions-urbaines/*" .&&. hasNoVersion)
                                      >>= sortByMdate))
                        <> listField "photos-small" (imageCtx :: Context ())
                           (reverse <$> (loadAll ("images/expressions-urbaines/*" .&&. hasVersion "small")
                                         >>= sortByMdate))
                        <> defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/photos.html" photosCtx
          >>= loadAndApplyTemplate "templates/default.html"
              -- the git date info is extracted for the directory
              -- containing the input images
              (gitDate' (const "images/expressions-urbaines")
               <> gitCommit' (const "images/expressions-urbaines")
               <> constField "title" "expressions urbaines"
               <> builtPageCtx)
          >>= relativizeUrls
      -- MAYBE  resize images so that they load faster.

    -- templates
    match "templates/*" $ compile templateCompiler

    -- CNAME, robots.txt, etc.
    match "assets/txt/*" $ do
        route $ gsubRoute "assets/txt/" (const "")
        compile copyFileCompiler

    create ["sitemap.xml"] $ do
      route idRoute
      compile $
        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" builtPageCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
                  { feedTitle = "Damien Couroussé"
                  , feedDescription = ""
                  , feedAuthorName = "Damien Couroussé"
                  , feedAuthorEmail = "no email"
                  , feedRoot = "http://damien.courousse.fr"
                  }

-- * Auxiliary compilers
--------------------------------------------------------------------------------

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
  \case
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

-- * Context builders
--------------------------------------------------------------------------------
builtPageCtx :: Context String
builtPageCtx =  constField "siteroot" (feedRoot feedConfiguration)
             <> listField "entries" builtPageCtx (loadAll $ "pages/*" .||. "posts/*")
             <> dateField "date" "%A, %e %B %Y"
             <> dateField "isodate" "%F"
             <> gitDate <> gitCommit
             <> defaultContext

postCtx :: Context String
postCtx =  dateField "date" "%B %e, %Y"
        <> dateField "dateArchive" "%b %e"
        <> modificationTimeField "mtime" "%F"
        <> defaultContext


-- | Extracts git commit info for the current Item
gitInfo
  :: String -- ^ the Context key
  -> String -- ^ the git log format string
  -> Context a
gitInfo  = gitInfo' (toFilePath . itemIdentifier)

gitInfo'
  :: (Item a -> FilePath) -- ^ a function that returns the path used for retrieving git info
  -> String -- ^ the Context key
  -> String -- ^ the git log format string
  -> Context a
gitInfo' f key logFormat = field key $ \item -> do
  unsafeCompiler $
    Process.readProcess "git" ["log", "-1", "HEAD", "--pretty=format:" ++ logFormat, f item] ""

-- | Extract the git commit date of the file sourcing the targeted
--   Item (context field: @gitdate@).
gitDate :: Context a
gitDate = gitDate' (toFilePath . itemIdentifier)
gitDate' :: (Item a -> FilePath) -> Context a
gitDate' f = gitInfo' f "gitdate" "%aD"

-- | Extract the git commit hash (short format) of the file sourcing
--   the targeted Item (context field: @gitcommit@).
gitCommit :: Context a
gitCommit = gitCommit' (toFilePath . itemIdentifier)
gitCommit' :: (Item a -> FilePath) -> Context a
gitCommit' f = gitInfo' f "gitcommit" "%h"

-- | last modification date of the file targeted by an item
mdateFile
  :: String -- ^ the context key
  -> Context a
mdateFile key = field key mdate

-- | last modification date of the file targeted by an item
mdate
  :: Item a
  -> Compiler String
mdate item = unsafeCompiler $ do
  let fp = toFilePath $ itemIdentifier item
  Process.readProcess "stat" ["-c", "'%y'", fp] ""

-- | sort items by last modification date
sortByMdate :: [Item a] -> Compiler [Item a]
sortByMdate = sortByM mdate
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

-- | the basename of the target file
basenameField :: String -> Context a
basenameField key = field key $ \item -> unsafeCompiler $
  -- TODO upgrade hakyll to v4.12.5.0: Update type of fromFilePath to use FilePath instead of String
  -- return $ basename $ toFilePath $ itemIdentifier item
  return $ takeBaseName $ toFilePath $ itemIdentifier item


-- | Lookup for an 'alt' description of the image.
--
--   The 'alt' description is read from a file with extension @.alt@.
--   The 'alt' file must be located in the same directory as the
--   target image, and must have the same filename except for the
--   extension name.
--   TODO If no file is found, return TODO.
--
--   For example, for an image file in @foo/image.jpeg@, the alt
--   description is read in file @foo/image.alt@.
--
--   In order to use this context in $if$ template constructs, we
--   actually generate two contexts: a field @key ++ "-description"@
--   that contains the alt description, and a field @key@ that is
--   actually a 'boolField' to be used in @$if$@ template constructs.
--
-- MAYBE the function is evaluated *each time* the field value is read.
imageAlt :: String -> Context a
imageAlt key = field key $ \item -> do
  let image = toFilePath $ itemIdentifier item
      -- the 'alt' file
      altFile = replaceExtension image "alt"
  mres <- unsafeCompiler $
    doesFileExist altFile >>= \case
      True ->  Just <$> readFile altFile
      False -> return Nothing
  case mres of
    -- we rely on the 'Compiler' instance of 'Alternative', so that we
    -- can use the field in an $if$ construct.  Taken from
    -- https://github.com/beerendlauwers/hakyll-extra/blob/cc4741a9781412108926ac2d7cf70f52a5ee68a3/src/Hakyll/Web/Template/Context/Extra.hs#L15
    Just x  -> return x
    Nothing -> empty

-- | Append the basename of the target route with @addn@.  @addn@ is
--   added before the last file extension.
-- >>> runRoutes (appendFileRoute "small") "image.jpeg"
-- "image.small.jpeg"
-- >>> runRoutes (appendFileRoute "small") "foo/image.jpeg"
-- "foo/image.small.jpeg"
-- >>> runRoutes (appendFileRoute "small") "foo/bar/image.jpeg"
-- "foo/bar/image.small.jpeg"
appendFileRoute :: String -> Routes
appendFileRoute addn = customRoute $
  -- TODO upgrade hakyll to v4.12.5.0: Update type of fromFilePath to use FilePath instead of String
  \i -> let f = toFilePath i
        in dropExtension f <.> addn <.> takeExtension f


-- * image processing
--------------------------------------------------------------------------------

type Width = Int
type Height = Int

-- | resize an image with imagemagick's binary @convert@.
--   Assuming the current Identifier is an image.
resizeImage :: Width -> Height -> Compiler (Item ())
resizeImage w h = do
  -- MAYBE.  check if input is an image.
  input <- getUnderlying
  output <- getRoute input >>= \case
    Just o -> return o
    Nothing -> fail $ "ERROR - route not found for" ++ show input
  siteRoot <- destinationDirectory . compilerConfig <$> compilerAsk

  unsafeCompiler $ do
    let fullRoute = siteRoot </> makeRelative "/" output
        resizeDims :: String
        resizeDims = show w ++ "x" ++ show h

    -- Hakyll's @makeDirectories@ does not work for our purpose: the
    -- directory is not created (... is not created soon enough?)
    createDirectoryIfMissing True $ takeDirectory fullRoute

    _ <- Process.readProcess "convert" [toFilePath input, "-resize", resizeDims, "-quality", "91", fullRoute] ""
    return $ Item input ()
