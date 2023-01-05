# Language Plugin

The goal of this project is to make prototypical implementation of a plugin
for the Glasgow Haskell Compiler (GHC),
such that the GHC can be used to compile programs that contain an implicit monadic effect.

## Compatibility

This plugin only works with GHC 9.2 and cannot be used with other versions.

## Examples
The plugin has been used to create two example languages.
- One Curry-Style functional logic language [curry-ghc-language-plugin](https://github.com/cau-placc/curry-ghc-language-plugin)
- One strict language with IO side effects (similar to ML) [ml-ghc-language-plugin](https://github.com/cau-placc/ml-ghc-language-plugin)

A fork of this has also been used to create a plugin for automatic function Inversion in Haskell. 
Details are in [this paper](https://dl.acm.org/doi/10.1145/3471874.3472982) and the project can be found at (https://github.com/cau-placc/inversion-plugin).

To capture all results of a nondeterministic computation, the user can import the computation in an ordinary Haskell module and use some of the following functions from `Plugin.CurryPlugin.Encapsulation` to get all results.

```haskell
data SearchMode = DFS | BFS

--   Examples:
--   >>> $(evalGeneric DFS 'someNullaryFunction)
--   >>> $(evalGeneric BFS 'someUnaryFunction  ) arg1
--   >>> (..)
evalGeneric :: SearchMode -> Name -> Q Exp

--   Examples:
--   >>> $(evalN 0) DFS someNullaryFunction
--   >>> $(evalN 1) BFS someUnaryFunction   arg1
--   >>> (..)
evalN :: Int -> Q Exp

--   Examples:
--   >>> eval DFS someNullaryFunction
eval  :: _ => SearchMode
      -> Nondet a -> [b]

--   Examples:
--   >>> eval1 BFS someUnaryFunction arg1
eval1 :: _ => SearchMode
      -> Nondet (a1 --> b1) -> a2 -> [b2]

--   Examples:
--   >>> eval2 BFS someBinaryFunction arg1 arg2
eval2 :: _ => SearchMode
      -> Nondet (a1 --> b1 --> c1) -> a2 -> b2 -> [c2]
```

## Examples and using the plugin in a sandbox

A sandbox project with examples is available to play around with in `sandbox/`. It can be loaded by executing `stack repl sandbox` from the root of the repository.

## Known Issues

 - Adding instances of derivable type classes to primitive types is not possible
 - Sharing in let-expressions does not work in some edge-cases
 - Using `:r` in GHCi only works on the second try
 - Type errors sometimes mention the effectful versions of type constructors
 - HIE and HaskellLanguageServer do not work  
 - ~Stack outputs some decoding failures while compiling the project. This can be ignored safely.~ Fixed with stack version 2.3.3

## Debugging

In order see, when my plugin generates invalid code, use the GHC option `-dcore-lint`. This type checks the generated core-code and emits an error message when something went wrong. I am very interested in finding those issues.
This option can also be turned on via `{-# OPTIONS_GHC -dcore-lint #-}`
