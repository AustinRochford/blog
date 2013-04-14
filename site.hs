--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char (toLower)
import           Data.Map (findWithDefault)
import           Data.Monoid
import           Hakyll


--------------------------------------------------------------------------------
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

    match "about.mkd" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= relativizeUrls

    --match "posts/*" $ do
    --    route $ setExtension "html"
    --    compile $ pandocCompiler
    --        >>= loadAndApplyTemplate "templates/post.html"    postCtx
    --        >>= loadAndApplyTemplate "templates/default.html" postCtx
    --        >>= relativizeUrls

    --create ["archive.html"] $ do
    --    route idRoute
    --    compile $ do
    --        let archiveCtx =
    --                field "posts" (\_ -> postList recentFirst) `mappend`
    --                constField "title" "Archives"              `mappend`
    --                context

    --        makeItem ""
    --            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --            >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList $ fmap (take 3) . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    context


--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list


--------------------------------------------------------------------------------
context :: Context String
context = activeContext `mappend` defaultContext

activeContext :: Context a
activeContext = mconcat $ map activeContextLink links

activeContextLink :: String -> Context a
activeContextLink link = field (link ++ "-active") $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    let title = map toLower $ findWithDefault "" "title" metadata
    return $ if title == link then "active" else ""

--it would be nice to automatically generate the menu from these
links = ["home", "about"]
