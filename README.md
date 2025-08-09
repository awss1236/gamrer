# Gamrer
This is a utility that allows for transforming Backus Naur Form
grammars into ~directly~ (not yet) usable parsers in haskell.
What makes it special is the fact that it (hopefully will) accepts
any combination of BNF, ABNF and EBNF that is to say it can handle
amalgamations of syntax because usually BNF is written to be read
by humans whilst machine readable BNF is limited to particular subsets.
The goal of Gamrer is to bridge the gap.

## Usage
The project uses pretty much the default cabal setup so just call
```
cabal run -- [bnf_file_path]
```
Which will automatically build and run the code and generate the
parser.

### Using the generated code
Making the generated code more usable is a big [TODO](#todos) for now.
But you can still copy paste the genertated code into an app
although I really wouldn't reccomned it (It's really ugly
right now).
You may also wrap it into a module.

## TODOs
- [ ] Make the interface better somehow (not too hard cause now it is currently non-existant).
  - [ ] Add support for prenamed types and constructors.
  - [ ] Add support for holding data within types.
  - [ ] Add support for prenamed tokens.
  - [ ] Make the outputted code a module.
- [ ] Extend the current grammar functionalities.
  - [ ] Make the ABNF/EBNF syntax supported.
    - [X] Add ability to lex all the different syntaxes.
    - [ ] Add ability to parse everything.
    - [ ] Add codegen.
