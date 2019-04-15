# The Arlia programming language

<img align="right" src="http://image.noelshack.com/fichiers/2018/47/6/1543061036-arlia-gemstone-2.png">

<img src="https://img.shields.io/badge/Made%20with-F%23-blue.svg?style=for-the-badge"> <img src="https://img.shields.io/badge/Build-In%20process-yellow.svg?style=for-the-badge"> <img src="https://img.shields.io/badge/Current%20State-Parser-red.svg?style=for-the-badge">

### Introduction

Arlia is a functional programming language, projecting a pragmatic development approach, adopting relatively new concepts, such as dependent types, refinement types, algebraic effects and effects, and providing for lazy, optimistic, and parallel evaluation.

### Goals

Arlia wants to be a pretty, pleasant and readable language offering many possibilities in the world of programming, adopting interesting new concepts to use for any type of project.
In the long term, a framework bringing together all the tools necessary for all purpose development is planned.

### Features

All the main features that will constitute the language.
Feel free to propose new ones!

  - Refinement types ;
  - Dependent types ;
  - Effects ;
  - Algebraic effects (= user defined effects) ;
  - Holes ;
  - Type methods, with accessibility (like OOP classes) ;
  - Optimistic execusion.

### Project status

Arlia is in full development. There may be some significant changes, but the language and its objectives will be met during the alpha versions.
The current version is **0.01**.

### The *Sharm* framework

The framework *Sharm* is a collection of mini-frameworks of all kinds for various uses, including elementary IO management tools, as well as GUI creation, also projecting a framework for artificial intelligence, via deep learning. Web tools are also being considered, as well as database management tools.
Arlia, and its elementary functions will of course be able to use this past of *Sharm*. But *Sharm* would be an essential tool for projects of all kinds.
Note: *Sharm* will be implemented after the language creation.

### Documentation

[Visit the project website](https://vanaur.github.io/Home.html) (not finished yet).

### About the author

I am a young student, very interested in the computer field, and especially in programming languages and artificial intelligence. I am alone on this project, but I sincerely hope that others will like it and that others will contribute to it.

### The `Nat` type, example

```idris
Nat = Zero | Succ Nat with  -- We define the type and its constructors
  toInt :: Integer          -- We append a method, called `toInt` that returns an `Integer` literal
  toInt =
    match it with           -- We match `it` (the data of `Nat` type instancied) with some patterns
    | 0 -> Zero
    | S n -> toInt n + 1
 
 
three'nat = Succ (Succ (Succ Zero)) -- We create a data of `Nat` type by using its constructors
three'int = three'nat.toInt         -- We create another data infered of `Integer` type, and call the method from
                                    --  the data defined above


str :: { nat: Nat | require nat.toString }
-- We create a data with a refined type that ask for a `Nat` type that
--  pocess the `toString` method. We can indicate the type of `toString`
--  but we will do something after as an example

printfn "int: %d" three'int    -- We use the `printfn` function that print something to screen
printfn "str: %s" ?str         -- We do the same, BUT we make an hole with the `str` (see above) value.
                               --  Indeed, this value is not created, just signed. The compiler will display
                               --  some informations about it (what is expected instead of the hole in fact).

kindOfType :: Boolean -> Type  -- We create a function that return a type such as its argument
kindOfType True  = Nat         -- If the argument is `True`, the type returned is `Nat`
kindOfType False = Integer     -- If the argument is `False`, the type returned id `Integer`

-- We define a new data with dependent type
kind :: (x: Boolean) -> (kindOfType x)
kind True  = Zero -- If `x` (paramter) is equal to `True`, we return `Zero` (from `Nat`)
kind False = 0    -- If `x` (paramter) is equal to `False`, we return `0` (from `Integer`)

```

### Contribute!

The language is still very young, maybe you have found some flaws in the design? Feel free to **fork** the project, or to suggest improvements or submit comments via the [project issues](https://github.com/vanaur/Arlia/issues).
