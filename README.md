CIS 194 Final Project
==============

CIS 194 Final Project - Lewis Ellis and Mitchell Stern

**Final Project Description**

For our final project, we built a Haskell platform for 2D board games such as TicTacToe, Connect 4, Chess/Checkers-type games, etc. Using associated data types, we created a completely general Game class whose instances constitute user-defined games. We also provided a library of functions that operate on this Game class, allowing users to play games as soon as they are defined. To demonstrate the extensibility of our platform, we provided sample implementations of several popular board games, and also defined AI strategies for a selection of these games.

**What We Learned and Accomplished**

First and foremost, this project helped us gain exposure to associated data types, thereby allowing us to create a generic Game class with our desired properties. Given a data type ```g```, we defined new data types ```Move g``` and ```State g``` inside of the Game class, then proceeded by defining a number of functions using these associated data types. Through the use of this structure, we ensure that functions for one game are applied only to states and moves associated with that game. Furthermore, this also allowed us to write general functions such as ```playGame``` and ```makeHumanPlayer``` that were applicable to all games, which in turn led to better modularity and less reuse of code.

In our experience, this aspect of the project was a success - once we had implemented the framework, it took less than an hour to define an entirely new game from start to finish. Furthermore, thanks to the functions included in our framework, no additional work was required to play the game from the console using a text-based interface.

Another feature of Haskell this project helped us learn more about is Haskell's treatment of IO. The main game loop prints the current state of the board, asks for a move (either from the user or from a computer player), performs the move on the board, and repeats until the game is over. Although the function that accomplishes this task is relatively short, writing it helped solidify the lessons we had learned in class about IO, monads, and user input in Haskell.

Once we had implemented the above features, we had accomplished most of what we set out to do in our final project. Getting to this point required roughly 5 hours of background reading for each of us, several meetings with Brent, and around 20 total hours of coding and debugging. Once we had the framework implemented, we then set out to implement a variety of board games in order to provide examples to potential users of our library.

We implemented four games in all: TicTacToe, Connect 4, Gomoku (Connect 5 on a Go board), and Breakthrough (a pawns-only Chess variant). After developing the framework and implementing TicTacToe, Gomoku was a trivial extension. Connect 4 required serious modifications to just two functions. Breakthrough is unlike any of the other games, but still required fewer than 20 additional lines of code to implement. 

We also made a heuristic negamax-based AI for TicTacToe which, thanks to the flexibility of our platform, converted into a Connect 4 AI with only a small handful of changes beyond replacing ‘TicTacToe’ with ‘Connect4’. Changing an array of heuristic valuations and a few dimension values was sufficient to produce a Connect4 AI which can play a human to a full board. The same extension was done for a Gomoku AI, but due to the size of the board and brutishness of the AI, the complexity is too high to play against it in real time.

In this way, we also learned about the often less-than-ideal complexity of Haskell computations. Though the AI as implemented is easier to think about in terms of its functional components, this leads to significant repeated calculations, unnecessarily reproducing the same state several times. There are certainly more efficient ways to write the same AIs in Haskell (extensively using Monads to avoid state reproduction), but this would have required more deviation from our game framework, which was the focus of the project.

Though we ran into trouble writing complex AIs between runtime limitations and the stateless nature of Haskell, we still feel our project was largely successful. The generic board game platform functioned exactly as expected, and implementing additional games proved quite simple once the framework had been written, which was our primary goal. We demonstrated this with three games of a similar genre, and one game that was very different from the others. In addition, using Haskell to implement AIs was a challenging but insightful endeavor. Haskell's functional nature helped us use the recursive definitions in typical AI strategies to our advantage, but the complexity involved in doing so also prevented us from producing AIs that played well in all situations. Therefore, while our AIs are moderately successful from our own testing, we hope to continue to improve upon their performance in the coming months.

**Running Instructions**

Playing a game can be accomplished by loading the appropriate file and running one of the following commands:

Tic-Tac-Toe (move input format: x y):
* Human vs. Human: ```playGame (makeHumanPlayer ticTacToeMoveParser) (makeHumanPlayer ticTacToeMoveParser)```
* Human vs. AI: ```playGame (makeHumanPlayer ticTacToeMoveParser) (ticTacToeAI)```

Connect 4 (move input format: column):
* Human vs. Human: ```playGame (makeHumanPlayer connect4MoveParser) (makeHumanPlayer connect4MoveParser)```
* Human vs. AI: ```playGame (makeHumanPlayer connect4MoveParser) (connect4AI)```

Gomoku (move input format: x y):
* Human vs. Human: ```playGame (makeHumanPlayer gomokuMoveParser) (makeHumanPlayer gomokuMoveParser)```
* Human vs. AI: ```playGame (makeHumanPlayer gomokuMoveParser) (gomokuAI)```
* Note: The Gomoku AI is very slow due to the branching factor; in its present state, it is not feasible to play against since it’s a simple expansion of the TicTacToe and Connect4 4 AIs.

Breakthrough (move input format: x y d, d = 1 2 3 forward directions):
* Human vs. Human: ```playGame (makeHumanPlayer breakthroughMoveParser) (makeHumanPlayer breakthroughMoveParser)```
