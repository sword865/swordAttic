{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Post where

import Import
import Text.Julius (RawJS (..))
import Database.Persist.Sql(fromSqlKey)

getPostR :: PostId -> Handler Html
getPostR postId= do
    (post, tags, comments) <- runDB $ do
        post <- get404 postId
        tags <- selectList [PostTagPost ==. postId] []
        comments <- selectList [CommentPost ==. postId] [Desc CommentPosted]
        return (post, tags, comments)
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentUserId, commentUserEmailId, commentUserSiteId) = commentIds
        setTitle . toHtml $ "悟剑阁" <> "-" <> postTitle post
        addScriptRemote "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        $(widgetFile "post")

commentIds :: (Text, Text, Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-createCommentUser", "js-createCommentUserEmail"
    , "js-createCommentUserSite")