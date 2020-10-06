-- New data type
data BookInfo = Book Int String [String] deriving (Show)
data MagazineInfo = Magazine Int String [String] deriving (Show)

-- Type synonyms
type CustomerID = Int
type ReviewBody = String

-- Type (constructor): BookReview 
-- Value constructor: BookReview
data BookReview = BookReview BookInfo CustomerID ReviewBody deriving (Show)

-- Algebraic types
type CardHolder = String
type CardNumber = String
type Address = [String]

-- Sum type
-- Type: BillingInfo
-- Value constructors: CreditCard, CashOnDelivery, Invoice
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- Record syntax
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

-- Recursive type
-- a is a type variable
data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x l) = x:fromList l

palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome [] = False
checkPalindrome [x] = False
checkPalindrome (x:xs) = (x == last xs) || (checkPalindrome $ init $ xs)