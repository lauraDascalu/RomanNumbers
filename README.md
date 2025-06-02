# Roman Numeral Parser and Evaluator

This program describes the **syntax** and **semantics** of Roman numerals in Haskell.

## Syntax of Roman Numbers

A Roman number is a sequence of letters: `I`, `V`, `X`, `L`, `C`, `D`, and `M`.

In Haskell, we define:
- a data type `RomanLetter` to represent each Roman numeral,
- a type synonym `RomanNumber` for a list of RomanLetter.

Parsing a string into the previously defined type `RomanNumber` involves:
- checking if the sequence of symbols represents a valid Roman numeral,
- if valid, returning a value of type `RomanNumber`.

## Semantics of Roman Numbers

Each Roman letter corresponds to an integer value. For a sequence of Roman letters, the rules to calculate the value are:
- If a symbol appears **after** a larger (or equal) symbol, its value is **added**.
- If a symbol appears **before** a larger symbol, its value is **subtracted**.

This behavior is possible because the data type `RomanLetter` is an instance of the type classes `Eq`, `Ord`, and `Show`, which provide equality, ordering, and string representation functionality respectively.
