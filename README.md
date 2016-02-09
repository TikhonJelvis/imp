# Analyzing IMP Programs

Some code for analyzing IMP programs with Z3, originally written for my [Compose 2016 talk](http://jelv.is/talks/compose-2016).

IMP is a *tiny* imperative language usually used to teach about formal semantics. Its operational semantics are particularly simple, which also makes it a great candidate to compile to Z3.

The code here is broken up into a few modules:
  * `Imp` contains the language's type definitions and an interpreter
  * `Parse` contains a *very simple* parser for IMP programs. It's extremely finicky: you have to have a semicolon after *every* command *except* for the last one, which *can't* have a semicolon.
    * Look to the example program `gcd.imp` to see how the syntax works.
  * `ImpToZ3` has a compiler that takes an IMP AST and shoves it into Z3. It includes helpers to run code *forwards* (ie use Z3 as an interpreter) and *backwards* (ie constrain the outputs and solve for inputs). It's fast backwards unless the output is invalid, in which case it hangs for a while depending on how far you unrolled the loops.
  * `Run` has some quick utility functions for playing with IMP: a REPL, a function to run a file through the interpreter and some utilities for compiling and running a Z3 formula from a file.
  
For now, the code is pretty rough, and a bunch of things (like how far to unroll loops) are just constants. To unroll loops a different amount, you have to change `bound` in `ImpToZ3`! Think of them as handy but hacky scripts rather than a full program.

If you have problems building this, please tell me. I've been using Nix for it (hence the `.nix` file), so I haven't set any bounds in my `.cabal` file. That'll probably cause problems down the line…

