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


The Inefficient Method
----------------------

Theo mentions that the normal algorithm for doing this is of the order 
of n to the power of k, where n is the size of the party and k is the
size of the clique. Let's implement this algorithm.

Firstly, let's find all possible ways to split the party into two: the 
clique will be one of these possibilities.


```haskell
import Data.List 

splitIntoTwo :: [a] -> [([a], [a])]
splitIntoTwo []     = []
splitIntoTwo xs     = zip subs $ reverse subs
                      where subs = subsequences xs
```

Note: This relies on a property of the `subsequences` function in Data.List that splits 
the list into a list of all possible subsequences of the original list.

```haskell
ghci> subsequences "abcd"
["","a","b","ab","c","ac","bc","abc","d","ad","bd","abd","cd","acd","bcd","abcd"]
```

The property is that if you concatenate the first and last element 
you get the original list back, if you concatenate the second element with the 
second last you also get the list back etc. Pairing the list with a reverse of itself
gives us what we need.

**NOTE: I have absolutely no idea if this is guaranteed. Is there a better way?**

Also note that the function `subseqs` on page 57 also exhibits this behaviour.

```haskell
ghci> subsequences "abcd"
["","a","b","ab","c","ac","bc","abc","d","ad","bd","abd","cd","acd","bcd","abcd"]
ghci> reverse $ subsequences "abcd"
["abcd","bcd","acd","cd","abd","bd","ad","d","abc","bc","ac","c","ab","b","a",""]

ghci> splitIntoTwo "abcd"
[("","abcd"),("a","bcd"),("b","acd"),("ab","cd"),("c","abd"),("ac","bd"),("bc","ad"),("abc","d"),
 ("d","abc"),("ad","bc"),("bd","ac"),("abd","c"),("cd","ab"),("acd","b"),("bcd","a"),("abcd","")]
```

There are duplicates of a sort - we have ("ab", "cd") and ("cd", "ab"), but that doesn't matter as 
we'll only check the first element of the tuple for clique-ness.

We use the definitions of clique-ness given at the start of the chapter: every celebrity knows every
other celebrity

```haskell
is_clique :: ([Person], [Person]) -> Bool
is_clique ([], _)   = False
is_clique (_, [])   = False
is_clique (cs, ps)  = and [p `knows` c | c <- cs, p <- ps] && and [not (c `knows` p) | c <- cs, p <- ps]

find_clique :: Party -> [Person]
find_clique p = head [cs | (cs, ps) <- splitIntoTwo p, is_clique (cs, ps) == True]
```

Let's test it on our party:

```haskell
ghci> find_clique party
[Celebrity "Cameron Diaz",Celebrity "Matt Damon",Celebrity "Tom Cruise"]
```

Code is in chap09b.hs.

