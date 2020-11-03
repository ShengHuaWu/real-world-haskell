import Control.Monad

-- Type definitions
data Context = Home | Mobile | Business deriving (Eq, Show)
type Phone = String

-- We don't want their business number, and we'd prefer to use their home number (if they have one) instead of their mobile number.
onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
-- onePersonalPhone alist = case lookup Home alist of
--     Nothing    -> lookup Mobile alist
--     Just phone -> Just phone
onePersonalPhone alist = lookup Home alist `mplus` lookup Mobile alist -- Use `mplus` from `MonadPlus` type class

-- Use this function with `filter`
isContext :: Context -> (Context, a) -> Bool
isContext c (c', _) = c == c'

-- Gather all business numbers (if they have one) or their mobile numbers
-- allBusinessOrMobilePhones :: [(Context, Phone)] -> [(Context, Phone)]
-- allBusinessOrMobilePhones alist = case filter (isContext Business) alist of
--     [] -> filter (isContext Mobile) alist
--     ns -> ns

allBusinessPhones :: [(Context, Phone)] -> [Phone]
-- allBusinessPhones alist = snd <$> allBusinessOrMobilePhones alist
allBusinessPhones alist = snd 
    <$>     filter (isContext Business) alist 
    `mplus` filter (isContext Mobile) alist 
