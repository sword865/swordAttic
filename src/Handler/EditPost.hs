{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EditPost where

import Import
import Utils.ArticlePost
import Handler.Edit (articlePostForm, toPostId)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Text (split)

postNewPostR :: Handler Html
postNewPostR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    ((result, widget), enctype)  <- runFormPost (articlePostForm Nothing)
    case result of
        FormSuccess atricle -> do
            now <- liftIO getCurrentTime
            let post = Post (atriclePostTitle atricle) now now "" 1 (atriclePostContent atricle)
            postId <- runDB $ insert post
            let postTags = map (\x-> PostTag postId x) $ split (==',') (atriclePostTags atricle)
            _ <- runDB $ forM postTags insert
            redirect $ PostR postId
        _ ->
            redirect CreatePostR

postEditR :: PostId -> Handler Value
postEditR postId = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    ((result, widget), enctype)  <- runFormPost (articlePostForm Nothing)
    case result of
        FormSuccess atricle -> do
            now <- liftIO getCurrentTime
            _ <- runDB $ update postId [PostTitle =. atriclePostTitle atricle, PostUpdated =. now,
                                        PostSummary =. "", PostContent =. (atriclePostContent atricle)]
            let postTagNames = split (==',') (atriclePostTags atricle)
            let postTags = map (\x-> PostTag postId x) postTagNames
            _ <- runDB $ deleteWhere [PostTagPost ==. postId, PostTagName /<-. postTagNames]
            _ <- runDB $ forM postTags insertBy
            redirect $ PostR postId
        _ ->
            redirect $ EditR postId
--     post <- requireJsonBody :: Handler Post
--     now <- liftIO $ getCurrentTime
--     let post' = post {postPosted = now, postUpdated = now}
--     insertedPost <- runDB $ insertEntity post'
--     returnJson insertedPost
