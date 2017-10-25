module Handler.Post where

import Import

postPostR :: Handler Value
postPostR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    post <- (requireJsonBody :: Handler Post)

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    -- maybeCurrentUserId <- maybeAuthId
    -- let comment' = comment { commentUserId = maybeCurrentUserId }

    insertedPost <- runDB $ insertEntity post
    returnJson insertedPost

