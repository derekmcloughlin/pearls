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
-- Everyone knows themselves
knows (Person p1) (Person p2) 
    | p1 == p2  = True
    | otherwise = False

tom = Celebrity "Tom Cruise"
cam = Celebrity "Cameron Diaz"
matt = Celebrity "Matt Damon"

john = Person "John Doe"
jane = Person "Jane Doe"
joe = Person "Joe Bloggs"
ernie = Person "Ernie"
bert = Person "Bert"

party = [bert, cam, ernie, joe, john, jane, matt, tom]


