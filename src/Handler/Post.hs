{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Post where

import Import

getPostR :: PostId -> Handler Html
getPostR postId= do
    (post, comments) <- runDB $ do
        post <- get404 postId
        comments <- selectList [CommentPost ==. postId] []
        return (post, comments)
    defaultLayout $ do
        setTitle . toHtml $ "悟剑阁" <> "-" <> postTitle post
        addScriptRemote $ "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        $(widgetFile "post")
