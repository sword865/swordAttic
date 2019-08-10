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


articlePostForm :: Html -> MForm Handler (FormResult ArticlePost, Widget)
articlePostForm extra = do
    (titleRes, titleView) <- mreq textField "title" Nothing
    (contentRes, contentView) <- mreq markdownField "content" Nothing
    (tagsRes, tagsView) <- mreq textField "tags" Nothing
    let articlePostRes = ArticlePost <$> titleRes <*> contentRes <*> tagsRes
    let widget = $(widgetFile "edit-form")
    return (articlePostRes, widget)


getEditR :: PostId -> Handler Html
getEditR postId = do
    (widget, enctype) <- generateFormPost articlePostForm
    let (title, content) = ("", "")
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
    (widget, enctype) <- generateFormPost articlePostForm
    defaultLayout $ do
        setTitle $ "悟剑阁" <> "-" <> "新建文章"
        $(widgetFile "edit")
