{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Monoid
import qualified Data.Set as Set
import Text.Pandoc.Options
import Hakyll

main :: IO ()
main = hakyll $ do
    match "site-src/static/css/**" $ route staticRoute >> compile compressCssCompiler

    match "site-src/static/favicon.ico" $ route staticRoute >> compile copyFileCompiler

    match "site-src/static/*.mkd" staticMarkdownRule

    match "site-src/resources/**" $ route baseRoute >> compile copyFileCompiler

    tags <- buildTags "site-src/posts/*" $ fromCapture "tags/*.html"

    match "site-src/posts/*" $ do
        route $ baseRoute `composeRoutes` setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "site-src/templates/post.html" (taggedPostCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "site-src/templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (const $ postList recentFirst)    `mappend`
                    constField "title" "Posts"                      `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "site-src/templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "site-src/templates/default.html" archiveCtx
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let tagCtx = constField "title" ("Posts tagged " ++ tag) `mappend` defaultContext

        route idRoute
        compile $ postsTagged tags pattern recentFirst
            >>= makeItem
            >>= loadAndApplyTemplate "site-src/templates/tag.html" tagCtx
            >>= loadAndApplyTemplate "site-src/templates/default.html" tagCtx
            >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        compile $ do
            let cloudCtx = constField "title" "Tags" `mappend` defaultContext

            renderTagCloud 100 300 tags
                >>= makeItem
                >>= loadAndApplyTemplate "site-src/templates/cloud.html" cloudCtx
                >>= loadAndApplyTemplate "site-src/templates/default.html" cloudCtx
                >>= relativizeUrls

    match "site-src/index.html" $ do
        route baseRoute
        compile $ do
            let indexCtx = field "post" $ const (itemBody <$> mostRecentPost)
            let homeCtx = constField "title" "Home" `mappend` defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "site-src/templates/default.html" homeCtx
                >>= relativizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"

            posts <- take 10 <$> (recentFirst =<< loadAllSnapshots "site-src/posts/*" "content")
            renderRss feedConfig feedCtx posts

    match "site-src/templates/*" $ compile templateCompiler


baseRoute :: Routes
baseRoute = gsubRoute "site-src/" (const "")

extensions :: Set.Set Extension
extensions = Set.fromList [Ext_inline_notes, Ext_raw_html, Ext_tex_math_dollars]

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration {
        feedTitle       = "AustinRochford.com",
        feedDescription = "Math, Data, and Software",
        feedAuthorName  = "Austin Rochford",
        feedAuthorEmail = "austin.rochford@gmail.com",
        feedRoot        = "http://austinrochford.com"
    }

mostRecentPost :: Compiler (Item String)
mostRecentPost = head <$> (recentFirst =<< loadAllSnapshots "site-src/posts/*" "content")

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = pandocCompilerWith pandocMathReaderOptions pandocMathWriterOptions

pandocMathReaderOptions :: ReaderOptions
pandocMathReaderOptions = defaultHakyllReaderOptions {
        readerExtensions = Set.union (readerExtensions defaultHakyllReaderOptions) extensions
    }

pandocMathWriterOptions :: WriterOptions
pandocMathWriterOptions  = defaultHakyllWriterOptions {
        writerExtensions = Set.union (writerExtensions defaultHakyllWriterOptions) extensions,
        writerHTMLMathMethod = MathJax ""
}

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "site-src/posts/*"
    itemTpl <- loadBody "site-src/templates/post-item.html"
    applyTemplateList itemTpl postCtx posts

postsTagged :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postsTagged tags pattern sortFilter = do
    template <- loadBody "site-src/templates/post-item.html"
    posts <- sortFilter =<< loadAll pattern
    applyTemplateList template postCtx posts

staticMarkdownRule :: Rules ()
staticMarkdownRule = do
    route $ staticRoute `composeRoutes` setExtension "html"
    compile $ pandocCompiler'
        >>= loadAndApplyTemplate "site-src/templates/default.html" defaultContext
        >>= relativizeUrls

staticRoute :: Routes
staticRoute = baseRoute `composeRoutes` gsubRoute "static/" (const "")

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = tagsField "tags" tags `mappend` postCtx
