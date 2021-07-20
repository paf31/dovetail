# runpurs-hs

PureScript corefn interpreter experiment for processing JSON.

Work in progress Haskell port of [runpurs](https://github.com/paf31/runpurs).

## Progress

- [ ] Basic interpreter
  - [x] Literals
  - [x] Pattern matching
  - [x] Closures
  - [x] Records
  - [x] ADTs
  - [ ] Recursive binding groups 
  - [ ] Guards
  - [ ] Multiple modules
- [ ] FFI
  - [x] Basic `FromValue` and `ToValue` classes
  - [x] `builtIn` function for constructing environments from Haskell functions
  - [ ] Generic deriving for `FromValue` and `ToValue`
- [ ] Integrated parser and type checker
  - [ ] Generate externs along with environment for foreign imports
- [ ] Improved error messages and error type
- [ ] CPS'd evaluator with `shift`/`reset`