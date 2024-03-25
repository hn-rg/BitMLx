# BitMLx

PoC implementation of a BitMLx -> 2xBitML compiler. This includes a compiler library and an example app that outputs the resulting BitML contracts as racket files, compatible with the existing [BitML compiler](https://github.com/bitml-lang/bitml-compiler).


## Project structure

The relevant folders in the project are:

- `src/`: source code for the compiler library.
- `test/`: unit tests (using `tasty-hunit`) for the compiler library.
- `app/`: source code for the example app, including examples  of haskell-embedded BitMLx contracts.
- `output/`: racket-embedded BitML contracts corresponding to the compilation results of the example contracts.

## Prerequisites

Before installing the BitMLx compiler, you should check if the following tools are installed on your device:

- `The BitML compiler`: Our BitMLx compiler is based on the original BitML compiler, so installing it first is essential. Follow the [BitML compiler instruction](https://github.com/bitml-lang/bitml-compiler) to install the tool.
- `LLVM`: [LLVM](https://llvm.org) enhances portability and optimization of Haskell programs. It is required by the [GHC](https://www.haskell.org/ghc/) compiler. To install `LLVM` on Ubuntu: run `sudo apt install llvm`
- `numa`: the library is required when linking the Haskell program. Install it on Ubuntu using: `sudo apt-get install libnuma-dev`
  

## Intructions

Install [stack](https://docs.haskellstack.org/en/stable/) and clone the repo. Then, you can run the following commands inside the project folder:

- `stack build` only builds the project. This is done implicitly for the other commands.
- `stack run` builds and runs the example app. This will write the output BitML contracts in the `output/` folder.
- `stack test` runs unit tests.
- `stack haddock --no-haddock-deps bitmlx --open` build the documentation and opens it in your web browser. Probably a good entry point!


## About the Unit Tests

Current unit tests have the following component:

- A module with an example BitMLx contract.
- A module with the expected Bitcoin BitML contract.
- A module with the expected Dogecoin BitML contract.
- A module with the testing code that compiles (and compares) the preconditions, build the compilation settings, compiles the contract and compares the outputs.


## TODOs

[x] Change priority choices to use global time formulas and implement timed priority choice.
[x] Implement the stipulation protocol.
[x] Implement the `reveal` primitive.
[x] Implement the participant authorization (signature) primitive.
[x] Adapt Chrysa's more complex examples to the new syntax.
[x] (maybe) Write a well-formed checker to ensure that a contract is valid before compiling.
[] (maybe) Build an integration script to further compile outputs to bitcoin transactions using the existing BitML compiler.