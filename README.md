# Straw

Compiling Haskell to cryptographic circuits.

# Approach

This defines a Cartesian closed category for [Snårkl](https://github.com/gstew5/snarkl) and provides tooling to work with the [Categorifier](https://github.com/con-kitty/categorifier) GHC plugin. Among other things, this can produce an R1CS in [libsnark](https://github.com/scipr-lab/libsnark) format.

# Usage

For tested correct code, see the examples. It is important to look at the build files as well to see how the plugin is enabled.

There are many more examples (for different categories) upstream in the [Categorifier repo](https://github.com/con-kitty/categorifier).
