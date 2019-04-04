# Source of the Arlia implementation

## Current step

- [x] Parser / AST:
  - [x] **First citizen types** (arrow type, type constructor, Type, flag type, dependent type, exclaimer type, tuple type, list type, record type, refined type) ;
  - [x] **Values** (object member call, implicit multiplication / power / both, constructor call, variable, literal, tuple, list, hole, exclaimer, record, condition, pattern-matching, lambda, do-notation, effect use, object reference) ;
  - [x] **Expressions** (applications, operators) ;
  - [x] **Statements** (import Arlia file, include external reference, using module, external function signature, effect creation, handler, class, function declaration (+ infix style + equational style), operator precedence / associativity statement, top-level condition, type declaration, module) ;
  - [ ] Good **error handler** ;
  - [ ] Good **comment support** ;
  - [ ] Better **indentation support** ;
  - [ ] Better **spaces support** ;
  - [ ] Better **identifier support** ;
  - [ ] **File importation**.
- [x] Desugger:
  - [x] Expressions / values ;
  - [x] Statements ;
  - [x] Modules ;
  - [ ] Types
- [ ] Type checker: **Next step**
- [ ] Codgen / optimizations: **Upcoming**