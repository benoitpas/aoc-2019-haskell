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

It is important to note that the error handling in the solution is minimal by choice, as the inputs are quite constraint I concentrate more on implementing an elegant solution (and learn new haskell features) than having an exhaustive error handling.

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

# Day 11
The first part is quite straightforward, it is a matter of reusing the IntCode interpreter and 'wire' it to handle a new type of input/output.

The second part is also fairly simple, the initial grid changes and the output needs to be displayed as an ascii grid.

# Day 12
Here using small functions to decompose the problem works really well. Actually, I even manage to implement a solution which could work with any number of dimensions, not just 3. As [Erik Meijer](https://en.wikipedia.org/wiki/Erik_Meijer_(computer_scientist)) said, [let the types guide you](https://twitter.com/headinthebox/status/543919077752729600). To store the point coordinates and velocity, I hesitated between arrays and list of lists. Finally I choose lists as they are better supported in Haskell and made the code more readable.

The main difficulty for part 2 is that a brute force approach cannot work to find the size of the repeat cycle. Luckily each dimension is independent, so we can find the cycle size for each dimension and then find the combined cycle. For that we used a 'reduce' function similar to the one developped in the solution for Day 3.

If part 1 had been written with part 2 in mind it could be better structured to reduce the number of transformations on the data structures, i.e. having findRepeat depend on findRepeatCol (instead of the reverse at the moment) and remove findRepeat2.

# Day 13
with the existing opcode interpreter, there are really no difficulties in the first part. I tried to use `where` in the solution as I'm quite comfortable with `let` as `let` is also present in the ML family of languages. I also find the `let .. in` a very elegant as it really makes it very explicit it is an expression.

Part 2 is really more involved ! Knowing the number of blocks to break is in the hundreds (c.f. part 1), either I can implement a full working version of the game or I can try to understand the logic of how points are counted. 

The first approach is interesting to learn how to do an interactive application with Haskell. In some ways it is the less risky but potentially the one that requires more work.

How quickly I can solve the problem with the second approach really depends on how complicated is the scoring mechanism.

Thinking about it, a third approach may be easier: this looks like a game of 'pong' so I could implement a logic where the 'paddle' follows the ball and let the program run until there are no bricks left. No visualization or user interaction required !

Actually as it wasn't that easy to write a logic that plays without fully understanding the game logic, I was faced with the choice of either reverse engineering the intcode to understand the it or write the UI for the game. As I have never written an interactive application with functional programming, I thought that would be a good learning experience.

Implementing the display for the game helped me better understand how the [IO monad](https://www.haskell.org/tutorial/io.html) works as well as what the ['do'](https://en.wikibooks.org/wiki/Haskell/do_notation) syntactic sugar does. For part 2 I have added a new entry point to the program (`run2`) that doesn't use the 'do' notation to have more flexibility to pass the IO monad around.

The program still loses. Thinking about it it may be easier to patch the program so that the ball always bounces, independently of the where the paddle is. When I did try simple patches, the program doesn't start (or at least for the time I waited nothing happened). I didn't check the code in details, but there may be some checksums on the integrity of the code.

So I didn't patch it in the end but I found the memory location where the future direction of the ball is stored. Using that information it was easy to guide the paddle to right direction.

This puzzle turned out to take a fair amount of time and multiple experiments. It helped me understand the IO monad a lot better as well as how the 'do {}' syntactical sugar is translated.

# Day 14

Part 1 seems quite straightfoward: build the graph and then compute the number of needed ORE on it. I hesitated between using a recursive algebraic data type or a 'Map' to store the graph.

It is a bit more complex to build the graph but it makes it easier to implement recursive algorithm while the map is easier to build but implementating various algorithms on it will be a bit more complicated.

For now as only one algorihm is required, I'm going to use the map. Also to store the counts I'm going to try to parametrize the types with 'Integral' be able to switch between 'Int' and 'Integer'. The only thing that wasn't immedialely obvious in the implementation was to use a map to keep track of 'unused' chemicals and update it after each reaction.

I don't know if it is psychological but I feed writing in haskell helps get the algorithm right with less iterations. After writing it for the first small example that returns '31', I tried it for the full puzzle and got the right answer straight away.

For part 2, I didn't try to implement a reverse of the computation of part 1 (it is probably very hard because of the dependencies between compounds) but instead used an iterative method with part 1.

# Day 15

Another puzzle with the IntCode interpreter. Initially I tried to randomly move the droid around to see what the area looks like (this is the run2 entry point). 

The area is actually a puzzle so the shortest path is the path to the oxygen equipment without going back. As the paths is also only 1 block wide, I simply implemented a backtracking logic that never goes back to its previous point. I do not keep track of the previously explored areas, only how many times the droid has moved.

After unlocking day 1, I realised I need the location of the oxygen equipment so I added it to the function. As there are 'loops' in the area, i.e.  gas can meet gas, the algorihm tracks where the gas has already been. Also, to mimic the gas propagation I used a breadth first exploration.

I didn't add any unit tests as I would have needed to 'mock' droid.

# Day 16

For Part 1 a simple brute force approach does it. Is really straightforward, especially as it is easy to use unit tests to verify the functions.
Reading Part 2 description, the brute force approach may not work !

Part 2 requires to look at what the transformation does (and read properly the instructions, more than usual). Here we can see that type of calculations is not the forte of Haskell, it would be faster with Scala or Python (not mentioning C++).

# Day 17

In this puzzle, the IntCode interpreter provides the input of the puzzle. That allows to write some unit tests. Initially I thought I would able to reuse the 'plot' function I implemented in Day15 to see what the scaffoldings look like. It worked but turned out to only display the part of the scaffolding that fits on the screen !

In part 2, we need to write instructions for the vaccum robot to explore all of the scaffolding. I'll first try to generate them by hand and automate the process if that proves too hard.