{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Edit where

import Import

getEditR :: PostId -> Handler Html
getEditR postId = do
        let (title, content) = ("", "")
        defaultLayout $ do
            setTitle . toHtml $ ("悟剑阁-"::Text)
            $(widgetFile "edit")