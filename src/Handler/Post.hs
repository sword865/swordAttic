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
        setTitle . toHtml $ postTitle post <> "'s User page"
        $(widgetFile "post")
--     muser <- maybeAuth
--     mCommentWidget <-
--       case muser of
--         Nothing -> return Nothing
--         Just _  -> generateFormPost (commentForm postId) >>= return . Just
--     extra <- getExtra
--     let pagename = extraPagename extra
--     (_, user) <- requireAuthPair
--     defaultLayout $ do
--         setTitle . toHtml $ userIdent user <> "'s User page"
--         $(widgetFile "profile")
