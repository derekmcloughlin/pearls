import Data.List 

data Person = Person String | 
              Celebrity String
              deriving (Show, Eq)

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

celebs :: [Person]
celebs = [cam, matt, tom]

party = [bert, cam, ernie, joe, john, jane, matt, tom]

subseqs [] = [[]]
subseqs (x : xs) = map (x:) (subseqs xs) ++ subseqs xs

splitIntoTwo :: [a] -> [([a], [a])]
splitIntoTwo []     = []
splitIntoTwo xs     = zip subs $ reverse subs
                      where subs = subseqs xs

is_clique :: ([Person], [Person]) -> Bool
is_clique ([], _)   = False
is_clique (_, [])   = False
is_clique (cs, ps) = and [p `knows` c | c <- cs, p <- ps] && and [not (c `knows` p) | c <- cs, p <- ps]

find_clique :: Party -> [Person]
find_clique p = head [cs | (cs, ps) <- splitIntoTwo p, is_clique (cs, ps) == True]

iff :: Bool -> Bool -> Bool
iff a b = (not a || b) && (not b || a)

nonmember :: Person -> [Person] -> Bool
nonmember p cs = and [p `knows` c && not (c `knows` p) | c <- cs]

member :: Person -> [Person] -> [Person] -> Bool
member p ps cs = and [x `knows` p && iff (p `knows` x) (x `elem` cs) | x <- ps]

cclique :: Party -> [Person]
cclique = head . ccliques

ccliques [] = [[]]
ccliques (p : ps) = map (p:) (filter (member p ps) css) ++ 
                    filter (nonmember p) css 
                    where css = ccliques ps

