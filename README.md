# Gamrer
This is a utility that allows for transforming Backus Naur form
grammars into directly usable parsers in haskell.

## Usage
Right now I don't ship an executable so you'll have to compile it,
which is not difficult as the program doesn't have any dependencies
outside of the standard libraries yet. (Consult `build.sh`)

After you've built it you can use it directly by giving it the path
of a file containing the grammar:
```
./main FILE
```
The Program outputs to stdout though so you may want to do a little
bit of shell magic:
```
./main FILE > OUT.hs
```
Where `OUT.hs` would then contain the output code.

### Using the generated code
Making the generated code more usable is a big [TODO](#todos) for now.
But you can still copy paste the genertated code into an app
although I really wouldn't reccomned it (It's really ugly
right now).
You may also wrap it into a module.

## TODOs
- [ ] Make the interface better somehow (not too hard cause now it is non-existant).
  - [ ] Add support for prenamed types and constructors.
  - [ ] Add support for holding data within types.
  - [ ] Add support for prenamed tokens.
  - [ ] Make the outputted code a module.
- [ ] Add a functional tokenizer.
- [ ] Extend the current grammar functionalities. (EBNF, ABNF, ...)
