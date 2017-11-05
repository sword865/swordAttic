{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Post where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Database.Persist.Sql(fromSqlKey)

getPostR :: PostId -> Handler Html
getPostR postId= do
    (post, comments) <- runDB $ do
        post <- get404 postId
        comments <- selectList [CommentPost ==. postId] [Desc CommentPosted]
        return (post, comments)
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentUserId, commentUserEmailId) = commentIds
        setTitle . toHtml $ "悟剑阁" <> "-" <> postTitle post
        addScriptRemote "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        $(widgetFile "post")

commentIds :: (Text, Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-createCommentUser", "js-createCommentUserEmail")