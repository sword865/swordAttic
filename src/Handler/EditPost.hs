{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EditPost where

import Import
import Text.Markdown (Markdown)
import Utils.ArticlePost


postNewPostR :: Handler Value
postNewPostR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    post <- requireJsonBody :: Handler Post
    now <- liftIO $ getCurrentTime
    let post' = post {postPosted = now, postUpdated = now}
    insertedPost <- runDB $ insertEntity post'
    returnJson insertedPost


postEditR :: PostId -> Handler Value
postEditR postId = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    post <- requireJsonBody :: Handler Post
    now <- liftIO $ getCurrentTime
    let post' = post {postPosted = now, postUpdated = now}
    insertedPost <- runDB $ insertEntity post'
    returnJson insertedPost
