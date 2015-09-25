[An explanation of the algorithm can be found here](https://elvishjerricco.wordpress.com/2015/09/24/generating-magic-decks-using-deck-building-strategies/).

You will need to have the Haskell platform installed.

Installation: `cabal install MTGBuilder`

Command line arguments:
---

`mtg-builder [args] input_deck_files...`

```
mtg-builder
  -o FILE    --output=FILE        Output file
  -i FILE    --input-seed=FILE    Input seed file. Only applicable for the additive algorithm
  -r NUMBER  --ranking=NUMBER     Order of rankings to compose the input decks with
  -f FILE    --ranking-file=FILE  File to save ranking information to (mostly for debug info)
  -v         --verbose            Enable verbose messages
  -s         --subtractive        Use the subtractive algorithm
  -V         --version            Print version
  -h         --help               Show help
```