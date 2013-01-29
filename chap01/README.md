Chapter 1 - The smallest free number
====================================

A naïve array-based solution
----------------------------
Original code (page 1)

    minfree :: [Nat] → Nat
    minfree xs = head ([0 .. ] \\ xs)

    (\\) :: Eq a -> [a] -> [a] -> [a] 
    us\\vs = filter(∉ vs) us

I don't like the use of mathematical symbols in code, so let's re-write as:

    minfree :: [Integer] -> Integer
    minfree xs = head([0..] \\ xs)

    (\\) :: Eq a => [a] -> [a] -> [a]
    us \\ vs = filter (notElem' vs) us

    notElem' :: Eq a => [a] -> a -> Bool
    notElem' a n = notElem n a

Haskell's notElem function works the other way around from ∉, so I've
defined the function notElem' that flips the arguments.

For easier testing I've also changed the data type to use Integer instead of Nat. More on
this later.

Testing this in ghci, we see that:

    gchi> :l chap01a.hs
    [1 of 1] Compiling Main             ( chap01a.hs, interpreted )
    Ok, modules loaded: Main.
    gchi> let a = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
    gchi> minfree a
    15

It works. The code is in chapt01a.hs.




