# Countdown Number Game Solver

## Build

Just run `make`. You will need `ghc` installed (Haskell compiler).

## Usage

```
$ ./countdown <list of numbers> <target>
```

Example:

```
$ ./countdown 6 10 5 1 4 37 649
Numbers: [6,10,5,1,4,37]
Target:  649

Results:

5 Number Solution:
10 + 5 = 15
37 + 6 = 43
43 * 15 = 645
645 + 4 = 649

6 Number Solution:
6 + 1 = 7
10 + 7 = 17
5 * 4 = 20
37 * 17 = 629
629 + 20 = 649
```

## How it Works

We start with our list of numbers, say `2,4,6,8`. I then find every possible
pair of numbers in this list, which in this case are:

- `4,2`
- `6,2`
- `8,2`
- `6,4`
- `8,4`
- `8,6`

Making sure to order them so that the first number is always larger than the
second. The reason is that for commutative operations like addition and
multiplication (the order of the operands doesn't matter), we don't need to
check both `4+2` and `2+4`, they are the same thing.

In the case of the non commutative operations like division and subtraction,
they only work if the first number is bigger than the second number anyways, so
`2-4` would be discarded anyways. So, there is no point in generated a list
where the first number is smaller.

Now we perform our 4 operations on each of these pairs:

- `4 + 2`
- `4 - 2`
- `4 * 2`
- `4 / 2`

- `6 + 2`
- `6 - 2`
- `6 * 2`
- `6 / 2`

- `8 + 2`
- `8 - 2`
- `8 * 2`
- `8 / 2`

- `6 + 4`
- `6 - 4`
- `6 * 4`
- `6 / 4` (Discarded because 6 does not divide evenly into 4)

- `8 + 4`
- `8 - 4`
- `8 * 4`
- `8 / 4`

- `8 + 6`
- `8 - 6`
- `8 * 6`
- `8 / 6` (Discarded because 8 does not divide evenly into 6)

I keep  track of each of these calculations as a string so I can recall them
later, and I make a new list replacing the used values with their calculated
value and mark them as calculated:

- `6,6,8`
- `2,6,8`
- `8,6,8`
- `2,6,8`

- `8,4,8`
- `4,4,8`
- `12,4,8`
- `3,4,8`

- `10,4,6`
- `6,4,6`
- `16,4,6`
- `4,4,6`

- `10,2,8`
- `2,2,8`
- `24,2,8`

- `12,2,6`
- `4,2,6`
- `32,2,6`
- `2,2,6`

- `14,2,4`
- `2,2,4`
- `48,2,4`

Now if the first value in any of these lists is the target value, and none of
the remaining values were calculated values (to eliminate solutions that
calculate a value and never use it), then we have a solution and we recall the
string of operations that got us here.

Now we repeat this whole process for each of our new lists, until we end up with
just lists of numbers with a single value, and then we are done.

I try to show a solution using 2, 3, 4, 5, and 6 of the numbers just for
curiosity, but there is no reason you couldn't just stop the whole program once
you find the first solution.

## References

I used [this](https://cgjennings.ca/articles/countdown-numbers/) to help with
creating the solver.
