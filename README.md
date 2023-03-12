# Working through HPLAR
This repository houses my reimplementation of John Harrison's Handbook of Practical Logic and Automated Reasoning in more modern OCaml. At the time of writing, it contains:

## Chapters 1 and 2
- A parameterized parser for the `formula` datatype using menhir. Menhir takes a `parameter` directive in an `.mly` file and produces a functor for a parser, which is pretty cool! 
- Pretty printers for propositional formulae.
- Printing of truth tables of propositional formulae. 