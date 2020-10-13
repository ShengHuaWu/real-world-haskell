-- ok: any number of fields and constructors
data TwoFields = TwoFields Int Int

-- A `newtype` can only have one value constructor, and that constructor must have exactly one field.
-- ok: exactly one field
newtype Okay = ExactlyOne Int

-- ok: type parameters are no problem
newtype Param a b = Param (Either a b)

-- ok: record syntax is fine
newtype Record = Record {
      getInt :: Int
}

-- bad: no fields
-- newtype TooFew = TooFew

-- bad: more than one field
-- newtype TooManyFields = Fields Int Int

-- bad: more than one constructor
-- newtype TooManyCtors = Bad Int | Worse Int

-- there's another important difference between data and newtype. 
-- A type created with the `data` keyword has a book-keeping cost at runtime, for example to track which constructor a value was created with. 
-- A `newtype` value, on the other hand, can only have one constructor, and so does not need this overhead. This makes it more space- and time-efficient at runtime.

-- The `data` keyword introduces a truly new algebraic data type.
-- The `type` keyword gives us a synonym to use for an existing type. We can use the type and its synonym interchangeably.
-- The `newtype` keyword gives an existing type a distinct identity. The original type and the new type are not interchangeable.
