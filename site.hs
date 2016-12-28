--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import qualified Data.Aeson.Types      as AT
import qualified Data.HashMap.Strict   as M
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (mappend)
import           Data.Text             (Text, pack, unpack)
import           Hakyll
import           System.FilePath.Posix
import           Text.Regex


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ metadataRoute (\m -> constRoute . valueToString . fromMaybe (error "Error!") $ M.lookup (pack "permalink") m)
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route (constRoute "archive")
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route (constRoute "home")
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
    match "includes/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

postRoute :: Routes
postRoute = customRoute makeR
    where
        makeR i  = shorten (toFilePath i) </> fileName (toFilePath i)

        fileName :: FilePath -> FilePath
        fileName p = case (convertPostFile . takeBaseName) p of
                         Just np -> np
                         Nothing -> error $ "[ERROR] wrong format: " ++ p
        shorten    = joinPath . tail . splitPath . takeDirectory

-- Removes date part from post file name.
convertPostFile :: String -> Maybe String
convertPostFile f = fmap last $ matchRegex postRx f

postRx :: Regex
postRx = mkRegex "^([0-9]{4})\\-([0-9]{2})\\-([0-9]{2})\\-(.+)$"

valueToString (AT.String s) = unpack s
