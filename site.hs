{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.Monoid
import qualified Data.Set as Set
import Text.Pandoc.Options
import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "textures/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "about.mkd" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    --for some reason, moving it this late gets the links right while putting it first doesn't
    tags <- buildTags "posts/*" $ fromCapture "tags/*.html"

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/post.html" (taggedPostCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (const $ postList recentFirst)    `mappend`
                    constField "title" "Archive"                    `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let tagCtx = constField "title" ("Posts tagged " ++ tag) `mappend` defaultContext

        route idRoute
        compile $ do
            postsTagged tags pattern recentFirst
                >>= makeItem
                >>= loadAndApplyTemplate "templates/tag.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
                >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        compile $ do
            let cloudCtx = constField "title" "Tags" `mappend` defaultContext

            renderTagCloud 100 300 tags
                >>= makeItem
                >>= loadAndApplyTemplate "templates/cloud.html" cloudCtx
                >>= loadAndApplyTemplate "templates/default.html" cloudCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "post" $ const mostRecentPost
            let homeCtx = constField "title" "Home" `mappend` defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" homeCtx
                >>= relativizeUrls

    match "resources/**" $ do
        route idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler

extensions :: Set.Set Extension
extensions = Set.singleton Ext_tex_math_dollars

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
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

postsTagged :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postsTagged tags pattern sortFilter = do
    template <- loadBody "templates/post-item.html"
    posts <- sortFilter =<< loadAll pattern
    applyTemplateList template postCtx posts

mostRecentPost :: Compiler String
mostRecentPost = (itemBody . head) <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = tagsField "tags" tags `mappend` postCtx
