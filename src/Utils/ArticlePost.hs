module Utils.ArticlePost where

import Data.Text (Text)
import Text.Markdown (Markdown)

data ArticlePost = ArticlePost
    { atriclePostTitle :: Text
    , atriclePostContent :: Markdown
    , atriclePostTags :: Text
    }
  deriving Show