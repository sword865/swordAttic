{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.CreatePost where
import Import


getCreatePostR :: Handler Html
getCreatePostR =
    defaultLayout $ do
        setTitle $ "悟剑阁" <> "-" <> "新建文章"
        $(widgetFile "createpost")
