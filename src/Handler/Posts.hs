{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Posts where

import Import
import Data.Foldable as F

_NUMBER_OF_ENTRIES_PER_PAGE_ :: Int
_NUMBER_OF_ENTRIES_PER_PAGE_ = 10

countPages :: Handler Int -> Handler Int
countPages hTotalPosts = do
    totalPosts <- hTotalPosts
    case totalPosts `divMod` _NUMBER_OF_ENTRIES_PER_PAGE_ of
        (n, 0) -> return n
        (n, _) -> return (n + 1)

countPosts :: Handler Int
countPosts = do
    users <- runDB $ selectList ([] :: [Filter Post]) []
    return (F.length users)

getPostsR :: Handler Html
getPostsR = do
    posts <- runDB $ selectList [PostStatus==. 1] [Desc PostPosted]
    defaultLayout $ do
        setTitle . toHtml $ ("悟剑阁-文章列表"::Text)
        $(widgetFile "posts")
