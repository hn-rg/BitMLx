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
- `LLVM`: [LLVM](https://llvm.org) enhances portability and optimization of Haskell programs. It is required by the [GHC](https://www.haskell.org/ghc/) compiler. To install `LLVM` on Ubuntu: `sudo apt install llvm`
- `numa`: [This library](https://man7.org/linux/man-pages/man3/numa.3.html) is required when linking the Haskell program. Install it on Ubuntu: `sudo apt-get install libnuma-dev`
  

## Intructions

Install [stack](https://docs.haskellstack.org/en/stable/) and clone the repo. Then, you can run the following command inside the project folder:

- `./BitMLx_pipeline.sh` executes the following pipeline:
  - Run the BitMLx compiler.
  - Replace hash placeholders in racket-BitML contracts.
  - Compile the racket-BitML contracts and store the outputs in new generated files with '.balzac' extension.


## About the Unit Tests

`stack test` runs unit tests.

Current unit tests have the following component:

- A module with an example BitMLx contract.
- A module with the expected Bitcoin BitML contract.
- A module with the expected Dogecoin BitML contract.
- A module with the testing code that compiles (and compares) the preconditions, build the compilation settings, compiles the contract and compares the outputs.


## Write your own BitMLx contracts

If you want to write your own BitMLx contracts: 

- `stack haddock --no-haddock-deps bitmlx --open` build the documentation and opens it in your web browser. Probably a good entry point!
