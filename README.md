# Non-Procedural Programming 

This repository contains a collection of exercises and solutions for topics in logic programming (Prolog) and functional programming (Lisp). The code demonstrates problem-solving using recursion, list processing, graph algorithms, interpreters, constraint satisfaction, and planning.

## Repository Structure

- **intro-lisp-exercises.lisp**  
  Solutions to introductory Lisp exercises, including recursive list utilities, subset generation, web page ranking, and graph-related problems.

- **fun-interpreter.lisp**  
  A simple interpreter for a functional language ("FUN") implemented in Lisp, supporting primitive and user-defined functions.

- **list-graph.pl**  
  Prolog predicates for list processing, filtering, substitution, clique detection in graphs, and string conversion tasks.

- **advanced-lisp-exercises.pl**  
  Advanced Prolog exercises, including course grading queries, cryptarithmetic puzzles, Sudoku solving, and reviewer assignment for papers.

- **ferry-planning/**  
  Directory containing Answer Set Programming (ASP) files for ferry transportation planning problems and a report on graph coloring experiments.
  - **ferry.lp**: Ferry planning domain definition.
  - **ferryInstance1.lp**, **ferryInstance2.lp**: Example problem instances for ferry planning.
  - **report.csv**: Results of graph coloring experiments.

## Running the Programs

### Lisp Files

- Use a Common Lisp implementation (e.g., SBCL, CLISP).
- Load the file in your Lisp REPL:
  ```lisp
  (load "intro-lisp-exercises.lisp")
  (load "fun-interpreter.lisp")
  ```
- Call the functions as described in the comments and docstrings.

### Prolog Files

- Use SWI-Prolog or a compatible Prolog interpreter.
- Load the file in your Prolog REPL:
  ```prolog
  ?- [list-graph].
  ?- [advanced-lisp-exercises].
  ```
- Run the predicates as described in the comments.

### ASP Files

- Use an Answer Set Programming solver such as `clingo`.
- Example usage:
  ```sh
  clingo ferry-planning/ferry.lp ferry-planning/ferryInstance1.lp
  ```

