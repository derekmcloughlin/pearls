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

Also note that the function `subseqs` on page 57 also exhibits this behaviour. I prefer to 
use a standard Haskell function if I can.

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

Anne's Improvement
------------------

Anne defines two functions: `member` and `nonmember` and uses these
with filters.

The code given in the book uses mathematical symbols, so:

```haskell
nonmember p cs = and [p knows c ∧ not (c knows p) | c ← cs]
```

becomes (replacing `∧` with `&&`):

```haskell
nonmember :: Person -> [Person] -> Bool
nonmember p cs = and [p `knows` c && not (c `knows` p) | c <- cs]
```

The `member` function is a bit messier. The use of `⇔ ` means 'if and only if'. 

```haskell
member p ps cs = and [x knows p ∧ (p knows x ⇔ x ∈ cs) | x ← ps]
```

"If and only if" is described [here](http://en.wikipedia.org/wiki/Iff). We define
a function `iff` as follows:

```haskell
iff :: Bool -> Bool -> Bool
iff a b = (not a || b) && (not b || a)
```

Thus the definition of member is:


```haskell
member :: Person -> [Person] -> [Person] -> Bool
member p ps cs = and [x `knows` p && iff (p `knows` x) (x `elem` cs) | x <- ps]
```

This makes the definition of `cliques` as follows:

```haskell
cclique :: Party -> [Person]
cclique = head . ccliques

ccliques [] = [[]]
ccliques (p : ps) = map (p:) (filter (member p ps) css) ++ 
                    filter (nonmember p) css 
                    where css = ccliques ps
```

Let's test it out:

```haskell
ghci> cclique party
[Celebrity "Cameron Diaz",Celebrity "Matt Damon",Celebrity "Tom Cruise"]
```

The code is in chap09c.hs.

Profiling the Results
---------------------

Let's profile the two solutions and look for the number of calls to `knows`:

```
ghc -O2 -prof -auto-all chap09c.hs
ghc -O2 -prof -auto-all chap09b.hs
./chap09b +RTS -p
./chap09c +RTS -p
```


Naive solution:

```
COST CENTRE                    MODULE                  no.     entries  %time %alloc   %time %alloc
chap09b.prof:  knows           Main                    102           0    0.0    0.6     0.0    0.6
chap09b.prof:     knows        Main                     93         559    0.0    0.0     0.0    0.0
```

Improved solution:

```
chap09c.prof:  knows           Main                    103           0    0.0    0.3     0.0    0.3
chap09c.prof:      knows       Main                    101          26    0.0    0.0     0.0    0.0
chap09c.prof:      knows       Main                     94          36    0.0    0.0     0.0    0.0
```

559 calls vs 62 calls is quite a difference. Remember there are only 8 people at the party.

Let's test it with a larger party. The file `names.txt` contains the top 25 boys 
names in Ireland in 2012.

Let's write a program that reads this list in and constructs a clique for whoever has a name
beginning with the letter 'M'. That's a clique of size 2 in a party of 25.

First, a function that returns a `Person` or `Celebrity` depending on the first letter of
their name.

```haskell
make_person :: Char -> String -> Person
make_person c s = 
    | head s == c   = Celebrity s
    | otherwise     = Person s

main = do
    handle <- openFile "names.txt" ReadMode
    contents <- hGetContents handle
    let irish_party = map (make_person 'M') $ lines contents
    putStrLn $ show $ cclique irish_party
```

Code in chap09d.hs. Running and profiling:

```
ghc -O2 -prof -auto-all chap09d.hs
./chap09d +RTS -p

     knows         Main                     99          65    0.0    0.0     0.0    0.0
     knows         Main                     94         106    0.0    0.0     0.0    0.0
```

171 calls.

If we replace `cclique` with `find_clique`:

```
    knows           Main                     94    34062376   33.2    0.0    33.2    0.0
```

Over 34 million calls.


More Improvements
-----------------

The final improvment makes the algorithm linear-time:

```haskell
cclique' :: Party -> [Person]
cclique' = foldr op []
op p cs 
    | null cs           = [p]
    | not (p `knows` c) = [p]
    | not (c `knows` p) = cs
    | otherwise         = p:cs
    where c = head cs
```

This reduces the number of calls further:

```
   knows           Main                     92          47    0.0    0.0     0.0    0.0
```

Trying this on a larger file of 200 boys and girls names, and using a larger clique 
of names beginning with 'S' we have:

Algorithm | Party Size | Clique Size | #calls
--------- | ---------- | ----------- | ------
cclique   | 100        | 7           | 1723
cclique'  | 100        | 7           | 194
cclique   | 200        | 14          | 6169
cclique'  | 200        | 14          | 394    

Note: if you try any of these with the original exponential algorithm you 
might be waiting a while. Even with a set of 50 and clique of 4 it takes
up a lot of memory, CPU and time.



