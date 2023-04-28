# Haskell Study

An alternative to using Quizlet to study vocabulary words.

## Usage

Place a `.txt` file in the same directoy as the executable, with the format:

```
term1\definition1
term2\definition2
```

then run the executable (it will ask for the filename). 

The program will then automatically run until you have correctly answered the 
definitions of all the specified terms in the `.txt` file.

Getting a word wrong enters "reinforcement mode", where you must type the 
definition of the term correctly 3 times before moving on to the next term 
(this does not count as correctly solving the term).

## Future Plans

The following features plan to be added:

 - More flexible syntax in the txt file to allow for multiple definitions per term.
 - An option to configure the number of iterations in reinforcement mode.
 - Ability to have multiple iterations of a `.txt` file (with config ability).
 - Printed statistics at the end to show how often terms were missed.
