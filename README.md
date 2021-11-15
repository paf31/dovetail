# Dovetail

Dovetail is a general-purpose PureScript interpreter written in Haskell.

It has excellent support for interop with Haskell code via PureScript's foreign function interface, along with a high-level Haskell API for integrating such code.

As such, it is possible to write low-level or domain-specific code in Haskell, and then to write the "glue code" in PureScript. In this way, Dovetail is a tool for reusing the frontend of the PureScript compiler (its syntax and type checker) to build domain specific languages in Haskell.

## Getting Started

To understand the library and how to use it, it is recommended that you read through the Haddock documentation, alongside the provided examples:

- Hackage documentation
  - [`dovetail` on Hackage](https://hackage.haskell.org/package/dovetail)
  - [`dovetail-aeson` on Hackage](https://hackage.haskell.org/package/dovetail-aeson)
  - [Haddocks on GitHub Pages](http://functorial.com/dovetail)
- Examples:
  - [JSON query language](https://github.com/paf31/dovetail/blob/main/examples/query-json/Main.hs)
  - [Fake data generator](https://github.com/paf31/dovetail/blob/main/examples/fake-data/Main.hs)
  
You can build the code and examples in this repository using `stack build`, and run the test suite with `stack test`.
