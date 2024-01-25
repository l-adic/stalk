# Straw

![CI](https://github.com/torsion-labs/straw/actions/workflows/nix-ci.yml/badge.svg)

Straw allos you to write Haskell and generate cryptographic circuits. Cryptographic circuits are often written in a low-level language, or a language that has been specifically tailored for such purposes and only superficially resembles a programming language the the programmer is familiar with. With Straw, a pure Haskell expression can be compiled to a circuit for evaluation and verification, supporting common zero-knowledge protocols via [Snarkl](https://github.com/torsion-labs/snarkl) and [arkworks](https://github.com/arkworks-rs/).

## Example

A tutorial is currently in-progress, but for an example it is instructive to compare the [sudoku solver](https://github.com/torsion-labs/straw/blob/sudoku/examples/Examples/Sudoku.hs) written in as pure Haskell expression, versus the one written in the [Snorkel DSL](https://github.com/torsion-labs/snarkl/blob/update-readme-with-lhs-example/tutorial/sudoku/Sudoku.md).  

# Approach

Straw implements a Cartesian closed category for [Snorkl](https://github.com/torsion-labs/snarkl) and provides tooling to work with the [Categorifier](https://github.com/con-kitty/categorifier) GHC plugin. This is a very general approach spearheaded by Conal Elliott's [concat](https://github.com/compiling-to-categories/concat) library. In general, one has to implement the typeclasses for an increasingly expressive cartesian closed category that matches the capability of the target, which in this case is Snorkel. The purpose of this library is to follow snorkel closely and to allow as general Haskell expressions as possible to be compiled to zero-knowledge circuits.

# Usage

For tested correct code, see the examples. Due to the utilization of the Categorifier as a GHC plugin, there are some limitations in what versions of GHC that is supported. Look for the example files as well as the nix files for correct usage.

There are many more examples (for different categories) upstream in the [Categorifier repo](https://github.com/con-kitty/categorifier).
