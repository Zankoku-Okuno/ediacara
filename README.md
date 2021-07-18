# Ediacara - portable assembler

I think I've hit a good level of abstraction here.
This is an assembler inspired by SSA, C--, and whatever else I've read about and have knocking around my head.
The key idea is to create a language/library that serves as an intermediate language for code generation.
A backend that reads these asm files is responsible for register allocation and operation selection.
The design is also meant to admit an implementation with an interpreter, rather than necessitating compilation.

A backend may also perform some type checking and optimizations, but those are meant to be done by a frontend that targets this lang.
In particular, control flow should already be optimized, and the type system included is present only to drive size calculations and generating call/return sequences.

While C is often describes as a portable assembler, standards-compliant C is lacking a number of useful assembly features.
In particular, alternate (perhaps even ad-hoc) calling conventions, tail calls, multiple return values, and alternate return continuations, and access to architecture-specific instructions are all missing.
