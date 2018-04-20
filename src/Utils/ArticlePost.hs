module Utils.ArticlePost where

import Data.Text (Text)
import Yesod.Form.Fields (Textarea)

data ArticlePost = ArticlePost
    { atriclePostTitle :: Text
    , atriclePostContent :: Textarea
    , atriclePostTags :: Text
    }
  deriving Show