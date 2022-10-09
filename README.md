# aoc-2019-haskell

Advent of Code 2019 in Haskell

![Build Status](https://github.com/benoitpas/aoc-2019-haskell/workflows/build/badge.svg)

I discovered Haskell by following  the course [Introduction to functional programming](https://www.edx.org/course/introduction-to-functional-programming) by the amazing [Erik Meijer](https://en.wikipedia.org/wiki/Erik_Meijer_(computer_scientist)). As I greatly enjoyed the course, I since then I have been thinking to learn more about Haskell.

What's better than trying to do the advent of code in the language ?

To run the tests, simply use:
```
$ stack test
```

To get the results for a given day, use the repl:
```
$ stack repl
ghci> Day1.run
3265923
```

# Day 1
This was mostly about setting up the environment and understanding how to use the unit test framework (which I didn't learn about in the course)

# Day 2
Here I mostly learnt how to use Array in Haskell, no issues with this puzzle. Compared to the Scala version (https://github.com/benoitpas/Advent-of-Code-2019/blob/master/src/main/scala/Day2.scala), it is a bit more verbose because of the array syntax (and I a lot more comfortable with Scala so the Haskell version can probably be improved a lot).

# Day 3
Here things start to get interested, the first solution I implemented does noy finish with the full scale puzzle. Without looking at it, I created a solution very similar to https://github.com/benoitpas/Advent-of-Code-2019/blob/master/src/main/scala/Day3.scala where I first create a list of points and convert it to a set to find the intersection.

For the first list (around 150000 elements), the conversion from List to Set does not finish after running for more than a minute.
Looking into the [documentation](https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Set.html), it turns out that balanced binary trees. It is probably better to use a [HashSet](https://hackage.haskell.org/package/unordered-containers-0.2.19.1/docs/Data-HashSet.html) instead.

Using the HashSet did not make much difference, so investigating how long it takes to convert a 10000, then 20000 and up to 50000 points list to set, it turns out there is also a memory issue.

When running Day3 with the following parameters, the solution is found in around 23 minutes:
```
stack repl --ghci-options="+RTS -M4095m -K4095m -RTS"
ghci> :set +s 
ghci> Day3.run
"puzzle 1:806"
(1404.46 secs, 3,139,229,792 bytes)
```

Out of curiosity I tried with a binary tree set and the time taken is in the same order of magniture:
```
(1393.68 secs, 3,103,167,544 bytes)
```

It probably makes sense to use smarter algorithm that calculate the segments intersections without generating all the points ;-).

And it turns out to be a lots more efficient too:

```
ghci> Day3.run
"puzzle 1:806"
(0.00 secs, 370,480 bytes)
```

# Day 4

Quite straightforward brute force solution

# Day 5
Continuing to implement the opcode interpreter from Day 2.
A good opportunity to introduce quickCheck tests. It was a bit laborious to add support for all opcodes but not really difficult.

# Day 6

For part 1, instead of using a brute force approach like in [Scala](https://github.com/benoitpas/Advent-of-Code-2019/blob/master/src/main/scala/Day6.scala) I decided to use a more subtle algorithm which recursively accumulate the results down the tree.

For part 2, the most efficient method would require to reverse the direction of the edges. Out of curiosity I tried a solution that doesn't, it iterates on the values to find the previous node. It turns out to perform fast enough for the given puzzled input.

# Day 7

Finally I was able to reuse 'nextStep' from Day5 with minimal changes. I did some refactoring to simplify a bit the code, especially around handling the parameters for the opcodes.
For the first part, adding the logic to initialise the amps and then run them 'connected' was quite straight forward with a fold.
For the second part it required one more level of loop to handle the feedback. Here using small functions to break down the logic really helped getting it right.

# Day 8

Solving Day 8 puzzles was quite straightforward, the solution is slightly different from Scala one because I'm learning the Haskell library as doing the excercises (I had a better knowledge of the Scala library when doing the other one).

# Day 9
The IntCode interpreter is back !
I started by changing it to handle long integer, which in Haskell is very easy, it was just about replacing 'Int' by 'Integer'. I introduced a type for the interpreter memory to make the code more dry.

Then I changed the memory type from Array to a Map to handle memory addresses which are outside of the initial memory. It would have been possible to keep using an array and grow it when needed. Instead I choose a map as with there is easier to manage. In contrast, with an array when the size grows I would have had to make it bigger. The issue with the map is that for 'continuous' memory it uses more than double the memory compared to an array as we need to store the index as well.

The rest of the puzzles was quite straightforward, there was still an issue with the implementation of one of the opcodes but that was quickly found by the self diagnostic program. The program in mode 2 takes surprisingly a few seconds to run.

# Day 10
Day 10 doesn't use the interpreter, it is a self contained puzzle. Here being able to write short functions to decompose the problem really helped as it was not very difficult but had lots of details to get right. Being able to unit test all the small independent functions allowed me to progress confidently.

For part 1, it is a simple combinational algorithm: for one point we find all aligned points with it and the reference astroid. Then we partitions the points in 2 sets, one containing the points 'before' the reference points and the points 'after'. Then from each set we keep the 'visible' one and add the others to the hidden points set. Then we repeat until we have processed all points.

The puzzle for part 2 hints to a more efficient solution: computing the angle and distances for points from the reference point. All aligned points have the same angle. The visible point for one angle is the one with the lower distance.

The issue is that if the angle is going to a 'double' and so even for aligned points [it may be slightly different](https://floating-point-gui.de/).

I thought that as we are dealing with integer coordinates, another option would be to represent the angle as a *pair of integer* like (x,y) where x and y have no common denominator so that the pair is unique for every angle of our grid. It turned out it is quite complicated to do all operations (including sorting) using these coordinates so I found a way to make the float angle computations deterministic: instead of find the angle for the direct coordinates (like finding the angle for point (-10,-5)), before finding the angle I simplify the pair like it was a fraction. With that example, I will compute the angle for (-2,-1). That means all points aligned with (-2,-1) will have the same float value for the angle.