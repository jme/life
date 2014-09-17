# lifes.ml

Just another version of Conway's Life game, written with OCaml in a (mostly) functional style.

A kind of Hello World tossed together to find my way around OCaml.

Fun & Lightweight, but not by any means meant to be a speed demon (for that you can use Golly or Xlife, etc).


## why

Why not? Also I wanted to become familiar with OCaml.

Anyway, even though it's been around forever Life still has not lost it's mesmerizing qualities.
Fire up as big a grid as your processor can quickly render; turn up some tunes; enjoy.


## background
Conway's famous 'zero-player' Life game needs little introduction.

This fairly standard implementation follows the classic B3/S23 format:

Cells in the grid live or die based on how many other live cells surround them.

* If a cell is live and has exactly 2 or 3 live neighbors then it survives for another cycle.
* If a cell is alive and has more than three live neighbors then it dies.
* If a dead cell has exactly 3 live neighbors then it springs to life.


![it looks like this](/snaps/fig1c.png)

 


Each cell in the grid is assigned an random state upon game start.

Edges are NOT wrapped. This results in some expected shaping of the cell states-space along edges.  


## howto
Run from command line, with optional grid size and zoom parameters, a few size & zoom presets and a few color scheme options.

two essential parameters: span and zoom

span: the Life gamespace is a square grid and the span parameter is just the size of an edge.
      e.g., a span of 200 yields a 200 x 200 game array
            a span of 1321 yields a 1321 x 1321 game array, etc.
      The population of the grid is then obviously = span<sup>2</sup>



zoom: individual cell edge size, in pixels. 



note: The required screen window size for a given game is (span * zoom)<sup>2</sup>
      e.g., for a span = 300 and zoom = 3 you'll get a 900x900 screen window.
      
And obviously a larger span means a greater population, slower framerates and so on.  Systems vary, YMMV.

Runs forever; stop with a keypress.



![snap2](/snaps/m600x600-1px-108g.png)


## brief dev notes

The Life grid is a square array of cells.

A cell is described by a cellrec, consisting of row, col, state info and a list of the sequence numbers for all neighboring cells.

These cell records are kept in a Map. I use a custom map with int keys.

In the map each cell record is keyed by a sequence number, calculated at runtime.


The cellrec map is initially filled, with each cell being set randomly to either live or dead.
During the creation of this initial map the neighbors list is populated.

On each generation/frame the map is recalculated. Probably not optimal.

Individual state computation uses the sequence number keys to quickly count live neighbors.

Using the standard Graphics lib, with basic double-buffer + flip.

caveats:
Several functions have too many params. There are many opportunities for mods and speedups.
I expect there are a bunch of neat lib functions I could have used here and there.

Also, I mostly use Clojure these days so I'm sure there are a few clojurisms that grate the ML-eye; sorry about that :-0

## There are two versions, an 'original' and a 'hybrid'


## 'original' version 

TL;DR 
  Cells are each assigned an integer 'sequence number' tag and cell info is kept in an integer-keyed map. For each cell valid cell neighbors are pre-calculated for fast lookup in said map. No mutation. 


### details
In the Life algorithm you need to reference the states of neighboring cells, a lot. So it seems a good idea to make such refernces fairly quick and/or cheap. I use a map with an integer key with a cell info record as the value.


First, some conventions:
  The grid is a N x N array, where N represents the 'span'.

  The 'sequence' numbering is arbitrarily set so that each  to run from 1 to (N x N), wrapping around row-to-col.

![seq](/snaps/seq.png)




  Cell info is held in a value of type 'cellrec', that includes a row value, a col value, a state value and a list of the sequence numbers of all the neighbors of the cell.


### startup prep
At startup the cellrecs are all populated:
  The coordinates of neighboring cells don't change, only their values do, so the sequence numbers of the valid neighbors of each cell are calculated and stashed in a 'neighbors' list.
  (edges are 'hard', with no wrap-around.)

  For each cell, column and row numbers are also filled in, along with a uniformly distributed random initial state
  (just uses the standard library random function)

  And then each cellrec is added to a sequence number-keyed Map.
  (Since the default Map uses string for keys I used a simple integer-keyed IntMap variant)

This map represents the inital state of the grid.


### run loop
Now it's just a matter of setting up a run loop that:
 renders the cell grid elements by drawing to a double-buffered Graphics screen, 

 flips the back buffer to the main screen context, 

 then calculates a new state value for each cell. 

 Then repeats.



### re-calculations
The calculation ops are direct and obvious; iterate through the grid Map:

For each cellrec you use the sequence numbers contained in that cell's neighbor list to lookup their cellrecs and count the number of live cells surrounding the current cell. 


Then, using the Game of Life rules, calculate a new value for this cell's state.

As this is a functional example, a new cellrec is then returned that contains the new state along with the other (unchanged) row, col and neighbor lit values.

Build a fresh grid array on each generation/frame;



## My code is my passport; profile me

The first version runs, but not too quickly. 

So, compile with profiling ON:   ocamlopt -p graphics.cmxa life.ml -o life

and run the app for a while to collect profiling data.

Then run:   gprof life
to see profiling information.

![original](/snaps/life-original-grpof.png)

The top couple of items are compare_val and camlMap.find, so it looks like the biggest costs at the moment are those map lookups. This is not really a surprise.

As the standard lib OCaml Map is backed by a balanced binary tree structure the best lookup time we can hope for is O(log(N)). This suggests a performance optimization...



#### Hybrid

This excercise is supposed to be an all-functional toy, but it's on the slow side and who among us can resist a few performance tweaks? 

Now you might suggest the perfectly reasonable use of a constant-time lookup (and item-mutable) Hashtbl instead of the Map. No need even to worry about table resizing costs since we know the size of the grid at startup.

While that would be a fine choice I instead jumped right back into imperative waters and used an Array to hold the cellrecs, with the state field of the cellrec marked as mutable and arranged so that the sequence number is the index.  

This way we speedup both state queries and updates and everything else stays the same, including the creation of a fresh new grid array for each generation. So, hybrid. 

The result is a 10-50x speedup, which is nice.


Another quick gprof round shows that the program's seq_scan function now occupies the top spot, followed closely by GC ops. 


![hybrid](/snaps/life-hybrid-grpof.png)

The seq_scan is pretty tight, being just a recursive pattern match loop, and apart from a few inconsequential type constraints there's not too much that this OCamel noob can see to do there within the existing logic structure.

And while a further optimization would be to drop that GC overhead down with a single all-mutable array for the frame buffer I think I'll stop here.




## Prerequisites `

Written with standard OCaml 4.01, no extra libs required

Linux i86 build also available.


## License

Copyright © 2014 jm ervin



