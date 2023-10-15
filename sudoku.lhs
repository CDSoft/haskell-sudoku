% Simple Sudoku Solver in Haskell
% Christophe Delord
% 26 May 2018

License
=======

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see http://www.gnu.org/licenses/.

Introduction
============

To practice Haskell I decided to port a small Sudoku solver I wrote in Python.
The solver is a basic brute force backtracking solver.

The solver is written in literate Haskell, which is cool for documenting a software.
The document is written in Literate Haskell and converted to Markdown with Pandoc.

> module Main where
>
> import Data.Char
> import Data.Array
> import Data.List
> import Data.List.Split
> import System.Environment
> import System.TimeIt

Sudoku representation
=====================

We will use a 4 dimension space to describe a Sudoku grid.
A grid is basically a 2D 9x9 array.
But a grid has a more complex structure. It has rows, columns and smaller 3x3 squares.
Rows, columns and 3x3 squares are in fact the intersections of a 4D plane
(row, column or 3x3 square) and a 4D hypercube (the whole grid).

We will see a grid either as a 2D array indexed by $\left(i,j\right) \in [0, 8]^2$,
either as a 4D array indexed by $\left(x,y,x',y'\right) \in [0, 2]^4$.

> type Indice4D = (Int,Int,Int,Int)
> type Indice2D = (Int,Int)
> type Digit = Int
> type Grid =  Array Indice4D Digit

$\left(i,j\right)$ are the coordinates of a digit in the big 9x9 grid.
$\left(x,y\right)$ are the coordinates of a small 3x3 square in the grid
and $\left(x',y'\right)$ are the coordinates of a digit in the $\left(x,y\right)$ small square.

The relation between $\left(i,j\right)$ and $\left(x,y,x',y'\right)$ is:

- $i = 3x + x'$
- $j = 3y + y'$

> ij :: Indice4D -> Indice2D
> ij (x,y,x',y') = (i,j)
>     where i = 3*x+x'
>           j = 3*y+y'

- $x = [i / 3], x' = i \% 3$
- $y = [j / 3], y' = j \% 3$

> xy :: Indice2D -> Indice4D
> xy (i,j) = (x,y,x',y')
>     where (x,x') = divMod i 3
>           (y,y') = divMod j 3

Input format
============

A grid is a file containing digits from 1 to 9 and underscores (`_`) for empty cells.
Internally a grid is a 4D array.

The function `parseGrid` turns a string into a 4D array.

> i4D = ((0,0,0,0),(2,2,2,2))   -- 4D indices
> r4D = range i4D               -- list of all the 4D indices
> i2D = ((0,0),(8,8))           -- 2D indices
> r2D = range i2D               -- list of all the 2D indices
>
> parseGrid :: String -> Grid
> parseGrid s = array i4D $ zip (map xy r2D) s'
>     where s' = map digit $ filter (not.isSpace) s
>           digit '_' = 0
>           digit d = read [d]

Inputs
======

The solver has three built-in grids.
These grids are supposed to be difficult
(well, you may find them difficult to solve with a pen).

The first one is called `easy`. I found it there:
<http://www.telegraph.co.uk/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html>

> easy :: Grid
> easy = parseGrid (" 8__ ___ ___ " ++
>                   " __3 6__ ___ " ++
>                   " _7_ _9_ 2__ " ++
>
>                   " _5_ __7 ___ " ++
>                   " ___ _45 7__ " ++
>                   " ___ 1__ _3_ " ++
>
>                   " __1 ___ _68 " ++
>                   " __8 5__ _1_ " ++
>                   " _9_ ___ 4__ ")

The second one - called `hard` - seems harder
(the solver takes longer to find the solution).
Shame on me, I don't remember where I found it.

> hard :: Grid
> hard = parseGrid (" 7_8 ___ 3__ " ++
>                   " ___ 2_1 ___ " ++
>                   " 5__ ___ ___ " ++
>
>                   " _4_ ___ _26 " ++
>                   " 3__ _8_ ___ " ++
>                   " ___ 1__ _9_ " ++
>
>                   " _9_ 6__ __4 " ++
>                   " ___ _7_ 5__ " ++
>                   " ___ ___ ___ ")

Some more grids also supposed to be very difficult for brute force algorithms.
In practice some of them can be solved very quickly...

<https://github.com/manastech/crystal/blob/master/samples/sudoku.cr>:

> worst1 :: Grid
> worst1 = parseGrid (" ___ ___ ___ " ++
>                     " ___ __3 _85 " ++
>                     " __1 _2_ ___ " ++
>
>                     " ___ 5_7 ___ " ++
>                     " __4 ___ 1__ " ++
>                     " _9_ ___ ___ " ++
>
>                     " 5__ ___ _73 " ++
>                     " __2 _1_ ___ " ++
>                     " ___ _4_ __9 ")

<https://app.crackingthecryptic.com/sudoku/P6phpMtQfN>:

> worst2 :: Grid
> worst2 = parseGrid (" 4__ ___ __2 " ++
>                     " __5 _82 9__ " ++
>                     " _2_ ___ _3_ " ++
>
>                     " __8 _1_ ___ " ++
>                     " 56_ _9_ _78 " ++
>                     " ___ _6_ 5__ " ++
>
>                     " _1_ ___ _6_ " ++
>                     " __6 15_ 7__ " ++
>                     " 3__ ___ __4 ")

<https://www.kristanix.com/sudokuepic/worlds-hardest-sudoku.php>:

> worst3 :: Grid
> worst3 = parseGrid (" 1__ __7 _9_ " ++
>                     " _3_ _2_ __8 " ++
>                     " __9 6__ 5__ " ++
>
>                     " __5 3__ 9__ " ++
>                     " _1_ _8_ __2 " ++
>                     " 6__ __4 ___ " ++
>
>                     " 3__ ___ _1_ " ++
>                     " _4_ ___ __7 " ++
>                     " __7 ___ 3__ ")

<https://www.mirror.co.uk/news/weird-news/worlds-hardest-sudoku-can-you-242294>:

> worst4 :: Grid
> worst4 = parseGrid (" __5 3__ ___ " ++
>                     " 8__ ___ _2_ " ++
>                     " _7_ _1_ 5__ " ++
>
>                     " 4__ __5 3__ " ++
>                     " _1_ _7_ __6 " ++
>                     " __3 2__ _8_ " ++
>
>                     " _6_ 5__ __9 " ++
>                     " __4 ___ _3_ " ++
>                     " ___ __9 7__ ")

The Sudoku grids are given on the command line.
`easy`, `hard`, `worst1` ... `worst4` are the built-in grids.
Other parameters are filenames.
A Sudoku file shall contain one grid with digits and underscores.
For instance:

~~~~~~~~~~~
7_8 ___ 3__
___ 2_1 ___
5__ ___ ___

_4_ ___ _26
3__ _8_ ___
___ 1__ _9_

_9_ 6__ __4
___ _7_ 5__
___ ___ ___
~~~~~~~~~~~

The `main` functions just solves each Sudoku grid given as parameters.

> main :: IO ()
> main = getArgs >>= mapM_ sudoku
>
> sudoku :: String -> IO ()
> -- builtin grids
> sudoku "easy" = solveGrid easy
> sudoku "hard" = solveGrid hard
> sudoku "worst1" = solveGrid worst1
> sudoku "worst2" = solveGrid worst2
> sudoku "worst3" = solveGrid worst3
> sudoku "worst4" = solveGrid worst4
> -- user defined grids
> sudoku filename = readFile filename >>= (solveGrid . parseGrid)
>
> solveGrid :: Grid -> IO ()
> solveGrid grid = do putStrLn "\nGrid:\n"
>                     putGrid grid
>                     timeIt $ do
>                         putGrids $ zip [1..] $ solve grid
>                         putStrLn ""

Output
======

The solutions are printed to stdout.

Currently only the first solution is printed
(printing all the solution may take a long time).

> putGrids :: [(Int,Grid)] -> IO ()
> putGrids ((i,g):gs) = do putStrLn ("\nSolution "++show i++":\n")
>                          putGrid g
>                          -- uncomment the following line to print all the solutions
>                          --putGrids gs
> putGrids [] = return ()

To print a grid we shall get its elements in the right order
(i.e. rows by rows, columns by columns).


> putGrid :: Grid -> IO ()
> putGrid g = putStr $ showGrid $ chunksOf 9 $ map ((g!) . xy) r2D

Lines are grouped by 3 and separated by an horizontal line.

> showGrid :: [[Digit]] -> String
> showGrid = intercalate "------+-------+------\n"
>          . map unlines
>          . chunksOf 3
>          . map showLine

Digits are also grouped by 3 and separated by a vertical line.

> showLine :: [Digit] -> String
> showLine = intercalate " | "
>          . map unwords
>          . chunksOf 3
>          . map (replace '0' '_' . show)
>
> replace :: Char -> Char -> String -> String
> replace c1 c2 cs = map repl cs
>     where repl c = if c == c1 then c2 else c

Solver
======

The solver is a brute force backtracking solver.
For every positions in the grid it tries all the possible digits.
It starts with the initial grid and tries to fill the first position $\left(0,0,0,0\right)$.
For each possible digit, it continues as this with the following positions
until it reaches the last one $\left(2,2,2,2\right)$.

`solve` tries to fill the grid at all positions (`r4D`), except for the positions
that already contain a non null digit.

> solve :: Grid -> [Grid]
> solve g = fillGrid' g ps
>     where
>         -- remove already filled positions
>         ps = filter (\p -> g!p == 0) r4D

`fillGrid` tries to fill in a cell. There are several cases:

- if all the cells have been filled in, we have found a solution
- otherwise `fillGrid` will recursively explore all the potential solutions
  by filling this cell with a "possible" digit
  and continuing with the remaining positions

> fillGrid :: Grid -> [Indice4D] -> [Grid]
> fillGrid g [] = [g]
> fillGrid g (p:ps) = concat [fillGrid (g // [(p, d)]) ps | d <- candidates g p]

`fillGrid` can be very slow because positions are always tested in the same order.
It would more clever to first try positions that have the minimum number of possible
candidates. `fillGrid'` is very similar to `fillGrid` but `p` is now the position that
has the minimum candidates.

> fillGrid' :: Grid -> [Indice4D] -> [Grid]
> fillGrid' g [] = [g]
> fillGrid' g ps = concat [fillGrid' (g // [(p, d)]) ps' | d <- qs]
>     where
>         (_, p, qs) = minimum [(length qs, p, qs) | p <- ps, let qs = candidates g p]
>         ps' = delete p ps

`candidates` lists all the possible digits in a cell
according to the current state of the grid.
A value is "possible" if it does not appear in the same row, column or small square.

The 4D representation of the Sudoku grid helps in defining rows, columns and squares.

The equation of the row containing $\left(a,b,a',b'\right)$ is

- $x \in [0, 2]$
- $y = b$
- $x' \in [0, 2]$
- $y' = b'$

In the same vein we can find the equations for the column and the square containing
a specific 4D point.

`candidates` lists all the digits that can be good candidates for a position.
Candidates are all the digits in $[1, 9]$ that are not in the row, the column
and the square that contain the position.

> candidates :: Grid -> Indice4D -> [Digit]
> candidates g (x,y,x',y') = [1..9] \\ (row++col++sqr)
>     where row = [n | x  <- r3, x' <- r3, let n = g!(x,y,x',y'), n /= 0]
>           col = [n | y  <- r3, y' <- r3, let n = g!(x,y,x',y'), n /= 0]
>           sqr = [n | x' <- r3, y' <- r3, let n = g!(x,y,x',y'), n /= 0]
>           r3 = [0..2]

Example
=======

Let's try to solve the `easy` Sudoku grid.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ runhaskell sudoku.lhs easy

@(script.bash ".build/sudoku easy")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After a few optimizations, the compiled version is pretty fast:

Grid     Time
------   --------
easy     @(script.bash [[ .build/sudoku easy | awk '$1 == "CPU" { print $3}' ]])
hard     @(script.bash [[ .build/sudoku hard | awk '$1 == "CPU" { print $3}' ]])
worst1   @(script.bash [[ .build/sudoku worst1 | awk '$1 == "CPU" { print $3}' ]])
worst2   @(script.bash [[ .build/sudoku worst2 | awk '$1 == "CPU" { print $3}' ]])
worst3   @(script.bash [[ .build/sudoku worst3 | awk '$1 == "CPU" { print $3}' ]])
worst4   @(script.bash [[ .build/sudoku worst4 | awk '$1 == "CPU" { print $3}' ]])

Tests made on a *@(script.bash [[LANG=C lscpu | grep "Model name" | sed 's/.*://' | sed 's/  */ /g']]:trim())*
powered by *@(script.bash [[. /etc/os-release && echo "$NAME $VERSION_ID"]]:trim())* and *@(script.bash [[stack ghc -- --version]]:trim())*.

Source
======

The Haskell source code is here: [sudoku.lhs](sudoku.lhs)

Feedback
========

Please let me know what you think about this way of coding/documenting?
Do you like my way of writing literate Haskell?
And the explanation about my implementation of a Sudoku solver?
