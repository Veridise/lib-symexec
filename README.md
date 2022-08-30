# lib-symexec: A library for symbolic execution utilities

<div><b>lib-symexec</b> is a library that provides a variety of utilities for building symbolic execution tools with <a href="https://docs.racket-lang.org/rosette-guide/index.html">Rosette</a>.</div>

## Features
- **Memory model**: Allows for both vectors and functions to be used as part of the memory model. Can distinguish between standard memory and storage variables. 
- **Symbolic rewrites**: Rewrite system for symbolic `ite` values, allowing easier use with Rosette's `for/all ([... #:exhaustive]) ...)` 
- **Logging**: Simple library for logging 

## Build and Install

After cloning the repo, from the `lib-symexec` directory:

```
cd ..
raco pkg create lib-symexec
raco pkg install lib-symexec.zip
rm lib-symexec.zip
rm lib-symexec.zip.CHECKSUM
```
