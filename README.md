# Haskell for Imperative Programmers

This is repository has haskell code that was built by following the video series
[Haskell for Imperative Programmers](https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV).

It was originally coded entirely in [REPL.IT](https://repl.it) which is an online virtual repl environment.

Since in order to complete the series the project requires the installation of external dependencies, 
the base `replt.it` template chosen was the [Haskell-Cabal](https://repl.it/@templates/Haskell-Cabal).

Check out the [template details](#template-details) for instructions on how to run it.

## Structure
Here's the general structure of the project highlighting the files and directories we are going to touch.

(Most files and directories generated by the scaffolding process from the template are ommited).

```
/
|-- Cabal-example.cabal # cabal configuration file
|                       # external dependencies, runtime flags
|
|-- Entrypoint.hs # program's Main entrypoint
|
|-- /src  # "lib" code of the program
    |
    |-- /Exercises  # solutions to the exercises proposed by the video series
    |
    |-- Utils.hs  # general utility (such as printing)
```

## Template details

This template makes it easy for you to use Haskell with Cabal on Repl.it. 

### Setup

- Fork this repl
- Open the shell (control/cmd + K) 
- Run `cabal sandbox init`
- Run `cabal update`
- Run `cabal install --only-dependencies`
- Hit the run button and see "Hello, Haskell"

### Usage

- Your entrypoint is Main.hs
- Run is linked to `cabal run`, so just hitting run would work!
- To add a new dependency simply add it to Cabal-example.cabal
- To install dependencies make sure to open the shell (control/cmd + K) and run `cabal update && cabal install --only-dependencies`

That's it! This template has `titlecase` as a dependency just for the example.
