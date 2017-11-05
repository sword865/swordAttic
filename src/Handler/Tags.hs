{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tags where

import Import

countElement::(Ord a) => [a] -> [(a , Int)]
countElement = map (\xs@(x:_) -> (x, length xs)) . group . sort

getTagsR :: Handler Html
getTagsR = do
    tags <- runDB $ selectList ([] :: [Filter PostTag]) []
    let tagNames = fmap (\(Entity _ tag) -> postTagName tag) tags
    let tagCount = countElement tagNames
    defaultLayout $ do
        setTitle . toHtml $ ("悟剑阁-文章列表"::Text)
        $(widgetFile "tags")