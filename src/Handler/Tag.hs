{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tag where

import Import
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

getTagR :: Text -> Handler Html
getTagR tag = do
     posts<- runDB
            $ E.select
            $ E.from $ \(post `E.InnerJoin` post_tag) -> do
                E.where_ (post_tag ^. PostTagName E.==. E.val tag)
                E.on $ post ^. PostId E.==. post_tag ^. PostTagPost
                E.orderBy [E.desc (post ^. PostPosted)]
                return
                    ( post ^. PostId
                    , post ^. PostTitle
                    , post ^. PostPosted
                    , post ^. PostSummary
                    )
     defaultLayout $ do
        setTitle . toHtml $ ("悟剑阁-文章列表"::Text)
        $(widgetFile "tag")