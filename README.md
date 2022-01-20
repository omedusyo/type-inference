# type-inference
Implementation of Typed (Polymorphic) Lambda Calculus together with type inference. I'm just trying to understand how the sausage is made, and it's fascinating.

What is implemented so far:
* Product, Sum, Arrow, Nat, Bool types, List (List : Type -> Type), and their constructors/eliminator (folds)
* Type inference (with let polymorphism, which so far was the hardest bit in all of this)
* Rudimentary module system
* Very basic UI (REPL) <https://omedusyo.github.io/type_inference/index.html>
* Custom parsing combinator library for the syntax
* Basic (pretty user hostile) syntax and a Parser for it
* Evaluator

In progress:
* Custom register machine simulator (adapting last chapter of [SICP](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html))
* Designing user defined inductive data types
* Designing a more powerful module system (trying to steal as much as possible from SML/OCaml)

Future:
* Implementing Garbage Collection/Compiler from scratch to the register machine language (maybe web assembly)
* Structure Editor
* Exploring Small Talk Image way of doing things
