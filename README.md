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
- [x] FFI
  - [x] Basic `FromValue` and `ToValue` classes
  - [x] `builtIn` function for constructing environments from Haskell functions
  - [x] Generic deriving for `FromValue` and `ToValue`
  - [ ] Handle newtypes properly at FFI boundary
- [x] Integrated parser and type checker
  - [ ] Generate externs along with environment for foreign imports
- [ ] Improved error messages and error type
- [ ] CPS'd evaluator with `shift`/`reset`
- [ ] Support CoreFn annotations for better error messages
- [x] Monadic interpreters