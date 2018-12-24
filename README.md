# The Arlia programming language

<img align="right" src="http://image.noelshack.com/fichiers/2018/47/6/1543061036-arlia-gemstone-2.png">

<img src="https://img.shields.io/badge/Made%20with-F%23-blue.svg?style=for-the-badge"> <img src="https://img.shields.io/badge/Build-In%20process-yellow.svg?style=for-the-badge"> <img src="https://img.shields.io/badge/Current%20State-Parser-red.svg?style=for-the-badge">

### Introduction

Arlia is a pragmatic functional and semi-object oriented programming language for general use and will benefit, in the future, from the hyper-framework "Charm", itself written in Arlia.

### Goals

Arlia wants to be a pretty, pleasant and readable language offering many possibilities in the world of programming, adopting interesting concepts to use for any type of project.

### Features

All the features that will constitute the language. When one of them is checked, it means that it has already been implemented in the compiler. Feel free to propose new ones!

<img src="https://img.shields.io/badge/Current%20State-Parser-red.svg?style=flat-square">

- **Type system**
  - [ ] Type inference ;
  - [ ] Type class-like ;
  - [ ] Algebraic types ;
  - [ ] Elementary types ;
  - [ ] Generic types ;
  - [ ] Interfaces ;
  - [ ] Static types.
- **Functions**
  - [ ] Higher order functions ;
  - [ ] Lambda functions ;
  - [ ] Asynchronous functions ;
  - [ ] Recursion.
- **Datas**
  - [ ] Immutability ;
  - [ ] Pattern matching ;
  - [ ] String interpolation ;
  - [ ] Matrix operations.
- **Other**
  - [ ] Exception handling ;
  - [ ] Driven event ;
  - [ ] Flow structure (`if`-`elif`-`else`, `while`, `do`-`while`, `for`)
  - [ ] Miscellaneous.
- **Envisaged features**
  - [ ] Linear types ;
  - [ ] Dependent type ;
  - [ ] Refinement types ;
  - [ ] Fillers (holes).

### Project status

Arlia is in full development. There may be some significant changes, but the language and its objectives will be met during the alpha versions.
The current version is **0.01**. Indeed, the language and its implementation are still very young.

### The *Charm* hyper-framework

The hyper-framework *Charm* is a collection of mini-frameworks of all kinds for various uses, including elementary IO management tools, as well as GUI creation and simulation / motion / sound elements, and also projecting a framework for artificial intelligence, via deep learning. Web tools are also being considered, as well as database management tools.
Arlia, and its elementary functions will of course be able to use this past of *Charm*. But *Charm* is an essential tool for projects of all kinds.
Note: *Charm* will be implemented after the language creation, be patient! 

### Documentation

[Visit the project documentation](https://arlia.gitbook.io/arlia-documentation/) (not finished yet).

### About the author

I am a young student, very interested in the computer field, and especially in programming languages and artificial intelligence. I am alone on this project, but I sincerely hope that others will like it and that others will contribute to it.

### Functional and object Hello World

```elm
import charm.dio

type Greeter name =
   let greet =
      printfn name

let greeter = new Greeter ("John")
greeter.greet()
```

### Contribute!

The language is still very young, maybe you have found some flaws in the design? Feel free to **fork** the project, or to suggest improvements or submit comments via the [project issues](https://github.com/vanaur/Arlia/issues).
