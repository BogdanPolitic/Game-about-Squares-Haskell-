# Game-about-Squares-Haskell-

The original game with graphical interface can be played at this web address: http://gameaboutsquares.com/

Player's task: at the end, each square must be in the exact same place as the circle of the exact color of the square. 

The code in the source has complete functionality about moving the square, and representing the whole 2D map scenario at each moving step.

I've chosen the simplest possible form of reproducing the map, making a matrix with flexible dimensions, in which columns are separated by "|". The map keeps its minimized form, meaning that if the distance between the farthest two elements is going to lower, the map dimensions will be adjusted the right way. The squares with arrows contain "v" "^" "<" or ">" between the "| |". The squares and the circles can be of one of the three colors: red, green or blue. Each color is represented by its beginning letter. The square color is an uppercase letter, while the circle color is a lowercase letter. For example, taking a blue square, its representation would be: "B". Taking a blue circle, the representation would be "b". 
The cell format of the matrix map is [SquareColorIfExists][CircleColorIfExists][ArrowIfExists], so it is 3 characters long. If the red square resides on the green circle, the cell representation would be "Rg ". If the blue square resides on an upwards (^) arrow, the cell representation would be "B ^" . Notice that there is an empty space between the "B" and the "^", because there is no circle in the exact position. This is a thing to remember about the cell representation: the square color is always on the 1st position, the circle color is always on the 2nd position, and the arrow type is always on the 3rd position.
One more thing to remember about the game is that there cannot be a square, a circle, and an arrow at the same time in one cell. At least one will miss. Also, there cannot be two elements of the same type on a cell, at a time (for example, there cannot be two squares on a specific cell, at a time).
The game is considered to be won when each square is residing in the same cell with the circle of the same color.

It is your job to create a proper custom 2D map, with elements at each position you want.
Example input for creating a new map: 
"level1 = addSquare Blue South (1, 0) $ addSquare Red North (3, 1) $ addCircle Blue (3, 0) $ addCircle Red (0, 0) $ emptyLevel"

"print level1"

The "North" and "South" are the arrow orientations, if in the position you specify, you want to (also) place an arrow. The arrow orientations can also be either "East" or "West".
The emptyLevel is the level in which there is no element, only the empty map. Therefore, the empty map is a 2D matrix of 0 size. This is the reason why "print emptyLevel" will show nothing, but a newline.

In the above example, at that specific level (which we called "level1"), notice that there is a square placed at position (1, 0) (which, by the way, means: line 1, column 0). If we do the following command: "move (1, 0) level1" , the new level is printed, with the blue square at (2, 0) position. The reason that the square moved 1 position downwards is that, previously in the "level1" level, there was also the arrow heading, which was pointing to "South" (downwards).
