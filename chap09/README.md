Chapter 9 - Finding Celebrities
===============================

Introduction
------------

It's much easier to see the problem using a data type:

```haskell
data Person = Person String | 
              Celebrity String
              deriving (Show)

type Party = [Person]

knows :: Person -> Person -> Bool
knows (Person p) (Celebrity c) = True
knows (Celebrity c) (Person p) = False
knows (Celebrity c1) (Celebrity c2) = True
-- Most people at the party don't know each other
-- except for Ernie and Bert
knows (Person "Ernie") (Person "Bert") = True
knows (Person "Bert") (Person "Ernie") = True
knows (Person p1) (Person p2) 
    | p1 == p2  = True      -- Everyone knows themselves
    | otherwise = False     -- but no-one else.

tom = Celebrity "Tom Cruise"
cam = Celebrity "Cameron Diaz"
matt = Celebrity "Matt Damon"

john = Person "John Doe"
jane = Person "Jane Doe"
joe = Person "Joe Bloggs"
ernie = Person "Ernie"
bert = Person "Bert"

party = [bert, cam, ernie, joe, john, jane, matt, tom]
```

Code in chap09a.hs.


