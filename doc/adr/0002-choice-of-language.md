# 2. Choice of Language

* Status: accepted
* Deciders: Fabien Boucher
* Date: 2021-04-12

## Context and Problem Statement

We want to process data from external sources to the monocle API.
Which language and style should the processing be implemented in?

## Considered Options

* Python
* Javascript/WebAssembly
* OCaml
* Haskell

## Decision Outcome

Chosen Option: "Haskell", because it comes out best (see below).

## Pros and Cons of the Options

### Python

* Good, because monocle API is already using the language.
* Bad, prone to runtime errors.

### Javascript/WebAssembly

* Good, because monocle WEB is already using the language/runtime.
* Bad, prone to runtime errors.

### OCaml

* Good, because industrial-strength.
* Bad, lack of tooling and ecosystem.

### Haskell

* Good, because advanced, purely functional programming language.
* Good, because high quality ecosystem.
* Bad, unfamiliar.
