{-# LANGUAGE OverloadedStrings #-}
module Handler.Comment where

import Import
import Data.Time.Clock(getCurrentTime)
import qualified Data.Text as T
import Text.Markdown

postCommentR :: Handler Value
postCommentR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    comment <- (requireJsonBody :: Handler Comment)
    now <- liftIO $ getCurrentTime
    let comment' = comment { commentPosted = Just now }
    insertedComment <- runDB $ insertEntity comment'
    returnJson insertedComment

--
-- getCommentR :: Handler Value
-- getCommentR = do
--     comment <- runDB $ selectList ([] :: [Filter Comment]) []
--     returnJson comment

