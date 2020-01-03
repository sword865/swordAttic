{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Edit where

import Import
import Utils.ArticlePost
import Yesod.Text.Markdown (markdownField)
import Database.Persist.Sql (toSqlKey, fromSqlKey)


articlePostForm :: Maybe ArticlePost -> Html -> MForm Handler (FormResult ArticlePost, Widget)
articlePostForm maybeArticle extra = do
    (titleRes, titleView) <- mreq textField "title" (atriclePostTitle <$> maybeArticle)
    (contentRes, contentView) <- mreq markdownField "content" (atriclePostContent <$> maybeArticle)
    (tagsRes, tagsView) <- mreq textField "tags" (atriclePostTags <$> maybeArticle)
    let articlePostRes = ArticlePost <$> titleRes <*> contentRes <*> tagsRes
    let widget = $(widgetFile "edit-form")
    return (articlePostRes, widget)


getEditR :: PostId -> Handler Html
getEditR postId = do
    (post, tags) <- runDB $ do
        post <- get404 postId
        tags <- selectList [PostTagPost ==. postId] []
        return (post, tags)
    let artTags = intercalate "," $ map (postTagName . entityVal) tags
    let article = ArticlePost (postTitle post) (postContent post) artTags
    (widget, enctype) <- generateFormPost $ articlePostForm (Just article)
    defaultLayout $ do
        setTitle . toHtml $ ("悟剑阁-"::Text)
        $(widgetFile "edit")

toPostId :: Int64 -> PostId
toPostId = toSqlKey

fromPostId :: PostId -> Int64
fromPostId = fromSqlKey

getCreatePostR :: Handler Html
getCreatePostR = do
    let postId = toPostId 0
    (widget, enctype) <- generateFormPost (articlePostForm Nothing)
    defaultLayout $ do
        setTitle $ "悟剑阁" <> "-" <> "新建文章"
        $(widgetFile "edit")
