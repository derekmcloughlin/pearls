Chapter 11 - Maximum Non-Segment Sum
====================================

First Approach
--------------

The first thing to note is that the list of non-segments of a list has more to 
do with the length of the list and the positions of elements in the list rather than
the actual data in the list.

So, the non-segments of the following lists:

```haskell
[-4, -3, -7, 2, 1, -2, -1, -4]
[1, 3, 5, -2, 4, 9, -11, -7]
```

always have the same positions. E.g. a list with indices [0, 1, 2, 3, 4, 5, 7] is a non-segment
for both lists.

To find the list of non-segments of a list we first construct
a list of all possible combinations

```haskell
markings :: [a] -> [[(a, Bool)]]
markings xs = [zip xs bs | bs <- booleans (length xs)]

-- Construct a list of all possible combinations of True or False for a list
-- of length n.
-- Note: The book uses booleans (n + 1) which isn't valid Haskell syntax
booleans 0 = [[]]
booleans n = [b : bs | b <- [True, False], bs <- booleans (n - 1)]
```

For example, for the list:

```haskell
testData :: [Int]
testData = [-4, -3, -7, 2, 1, -2, -1, -4]
```

, which is length 8, we have 2^8 (256) combinations:

```haskell
ghci> booleans 8
[
    [True,True,True,True,True,True,True,True],
    [True,True,True,True,True,True,True,False],
    [True,True,True,True,True,True,False,True],
    [True,True,True,True,True,True,False,False],
    ...
]
```

Zipping this with the list gives

```haskell
ghci> markings testData
[
    [(-4,True),(-3,True),(-7,True),(2,True),(1,True),(-2,True),(-1,True),(-4,True)],
    [(-4,True),(-3,True),(-7,True),(2,True),(1,True),(-2,True),(-1,True),(-4,False)],
    [(-4,True),(-3,True),(-7,True),(2,True),(1,True),(-2,True),(-1,False),(-4,True)],
    [(-4,True),(-3,True),(-7,True),(2,True),(1,True),(-2,True),(-1,False),(-4,False)],
    ...
]
```

Remember that the non-segments depends on the indices in a list. So the non-segments
of the following lists:

```haskell
[-4, -3, -7, 2, 1, -2, -1, -4]
[1, 3, 5, -2, 4, 9, -11, -7]
```

have the same indices. A list with indices [0, 1, 2, 3, 4, 5, 7] is a non-segment for both.

We can represent this as:

```haskell
[True, True, True, True, True, True, False, True]   -- i.e. element with index 6 is missing.

or 

TTTTTTFT
```

Using this representation the regular expression

```shell
F*T+F+T(T + F)*
```

is used to find all non-segments.  In Vim-style this becomes:

```shell
\v^F*T+F+T[T,F]*$
```

You can test this out in a text editor by generating the complete list of representations:

```haskell
ghci> [map (\x -> if x then 'T' else 'F') b | b <- booleans 8]
["TTTTTTTT","TTTTTTTF","TTTTTTFT","TTTTTTFF", ....
```

The important bit of the regular expression is the part that states there 
must be at least one 'F' between some 'T's.

```shell
F*T+F+T(T + F)*
  ^---^
```

In fact, this is really all you need. However, the RE above will match the entire expression.

Converting a regular expression to a finite state automaton is a bit beyond me. However, 
on [HackingOff](http://hackingoff.com/compilers/regular-expression-to-nfa-dfa)
there's a tool that will do some of this.

The finite state automaton representing the regular expression above is just a simple state
machine going through the three parts of the expression:

State | Meaning
----|---------
 E  | The first (or none) F's in the sequence.
 S  | The following T's (one or more)
 M  | The following F's (one or more)
 N  | The following T (one)


"N" is the final state - any more T's or F's after that aren't important. If a 
sequence of T's and F's gets to this state, then we know it's a non-segment.

The state diagram looks like this:

![](https://github.com/derekmcloughlin/pearls/blob/master/chap11/state-machine.png)

The states can be described using a Haskell data type, and the transitions can be 
encoded using the `step` function:

```haskell
data State = E | S | M | N
    deriving(Eq, Show)

step :: State -> Bool -> State
step E False = E 
step E True = S 
step S False = M 
step S True = S 
step M False = M
step M True = N
step N False = N
step N True = N
```

To apply these transitions to a sequence with markings, e.g. FFTTTFTF, we use a fold:

```haskell
nonseg :: [(a, Bool)] -> Bool
nonseg = (== N) . foldl step E . map snd
```

This will start with applying `step E` to 'False', yielding state 'E' again.
When it reaches the first 'True', then the `step E` gives the state 'S'.
When it goes through the next 'True's, the state stays at 'S'.
The next 'False' moves the state to 'M'.
The next 'True' moves the state to 'N'.
After that we don't care - any True or False keeps the state at 'N'.

We finally have the code to find the maximum sum of a non-segment:


```haskell
mnss :: [Int] -> Int
mnss = maximum . map sum . nonsegs
```

Code is in chap11a.hs.






