{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EditPost where

import Import
import Text.Markdown (Markdown)
import Utils.ArticlePost
import Handler.Edit (articlePostForm, toPostId)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Text (split)

postNewPostR :: Handler Html
postNewPostR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    ((result, widget), enctype)  <- runFormPost articlePostForm
    case result of
        FormSuccess atricle -> do
            now <- liftIO getCurrentTime
            let post = Post (atriclePostTitle atricle) now now "" 1 (atriclePostContent atricle)
            postId <- runDB $ insert post
            let postTags = map (\x-> PostTag postId x) $ split (==',') (atriclePostTags atricle)
            runDB $ forM postTags insert
            redirect $ PostR postId
        _ ->
            defaultLayout $ do
                let postId = toPostId 0
                setTitle $ "悟剑阁" <> "-" <> "新建文章"
                $(widgetFile "edit")
--             redirect CreatePostR

postEditR :: PostId -> Handler Value
postEditR postId = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    post <- requireJsonBody :: Handler Post
    now <- liftIO $ getCurrentTime
    let post' = post {postPosted = now, postUpdated = now}
    insertedPost <- runDB $ insertEntity post'
    returnJson insertedPost
