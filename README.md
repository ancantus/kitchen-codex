# :amphora: Kitchen Codex
An kitchen management software prototyped by my wife's excel spreadsheet.

Built using a HTMX frontend and Ocaml backend with the goals of:
1. Re-learn frontend development.
2. Learn a functional programming language (Ocaml)
3. Make it easy enough to use that my wife wants to use it instead of the spreadsheet.

## Features
### Minimum Features:
* Run on Windows & Linux as a self-hosted webpage with no external dependencies (I like desktop apps)
* Display and edit a weekly meal plan drawing from a bank of stored meals
* Diplay and edit meals with specified ingredients.
* Generate a shopping list for the week for the meals planned.

### Feature Ideas:
* Display and edit a 'pantry contents' list of ingredients: and remove that from the shopping list.
* Scale meals by family / group size to model hosting.
* Display and edit recipe instructions alongside meals.
* Track variants of meals (deltas from a base meal) to make meal input easier.

## Quickstart
1. Build the code using the build script
```
./build.sh 
```
2. Run the server using the run script
```
./run.sh
```
