[An explanation of the algorithm can be found here](https://elvishjerricco.wordpress.com/2015/09/24/generating-magic-decks-using-deck-building-strategies/).

This program is not currently uploaded on cabal.

To run, you'll need to have haskell installed. Create a cabal sandbox, or don't. That's up to you. Run `cabal install`. To run, use the `mtg-builder` tool installed by cabal.

Command line arguments:
---

`mtg-builder [args] input_deck_files...`

* `-o FILE` | `--output=FILE`
    * Output file
* `-r NUMBER` | `--ranking=NUMBER`
    * Order of rankings to compose the input decks with
* `-f FILE` | `--rankingFile=FILE`
    * Order of precision to measure interactions with
* `-v` | `--verbose`
    * Enable verbose messages
* `-V` | `--version`
    * Print version
* `-h` | `--help`
    * Show help