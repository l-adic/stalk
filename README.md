# Stalk

![CI](https://github.com/l-adic/stalk/actions/workflows/nix-ci.yml/badge.svg)

Stalk allows you to write a pure Haskell program and generate a system of arithmetic constraints corresponding to that program. Such constraint languages are often written in terms of a low-level "gadget libraries", or a language that has been specifically tailored for such purposes and only superficially resemble a "normal" programming language. Other approaches attempt to take advantage of existing compilers targeting e.g. RISC-V, but the resulting compiled programs are not "first class" expressions in the language. With Stalk, a pure Haskell expression can be compiled to an equivalent constraint representation which is first class in the Haskell language. These expressions can be futher compiled for use in zero-knowledge protocols using [Snarkl](https://github.com/l-adic/snarkl) and [arkworks](https://github.com/arkworks-rs/).

## Example

A tutorial is currently in-progress, but it is instructive to compare the [sudoku solver](https://github.com/l-adic/stalk/blob/sudoku/examples/Examples/Sudoku.hs) written in as pure Haskell expression, versus the one written in the [snarkl DSL](https://github.com/l-adic/snarkl/blob/main/tutorial/sudoku/Sudoku.md).  

# Approach

Stalk implements a Cartesian closed category for [snarkl](https://github.com/l-adic/snarkl) and is meant to build with the [Categorifier](https://github.com/con-kitty/categorifier) GHC plugin. This is a very general approach spearheaded by Conal Elliott's work [Compiling to Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf). In general, one has to implement the typeclasses for an increasingly expressive cartesian closed category that matches the capability of the target, which in this case is snarkl. The purpose of this library is to follow snarkl closely and to allow as general Haskell expressions as possible to be compiled to constraint languages.

# Usage

For tested correct code, see the examples. Due to the utilization of the Categorifier as a GHC plugin, there are some limitations in what versions of GHC that is supported. Look for the example files as well as the nix files for correct usage.
