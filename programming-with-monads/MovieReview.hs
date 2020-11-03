import Control.Monad

-- Type definition
data MovieReview = MovieReview {
    revTitle :: String
  , revUser :: String
  , revReview :: String
}

-- Unwrap one layer of `Maybe` for `lookup`
myLookup :: Eq a => a -> [(a, Maybe b)] -> Maybe b
myLookup key alist = case lookup key alist of
    Just (Just value) -> Just value
    _                 -> Nothing

-- Cannot use `lookup` directly, because it will create nested `Maybe`
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
-- simpleReview alist = do
--     title <- myLookup "title" alist
--     user <- myLookup "user" alist
--     review <- myLookup "review" alist
--     return (MovieReview title user review)
-- simpleReview alist = liftM3 MovieReview (myLookup "title" alist) (myLookup "user" alist) (myLookup "review" alist) -- `liftM3` :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
simpleReview alist = MovieReview <$> myLookup "title" alist <*> myLookup "user" alist <*> myLookup "review" alist -- Use `Applicative`
