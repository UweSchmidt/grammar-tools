# grammar-tools

Some tools for processing context free grammars.
Used in a compiler course for demonstrating computation of
First- and Follow-sets and LL(1) parser tables

Installation can be done via haskell stack.

1. Install stack (https://docs.haskellstack.org/en/stable/README/)
2. stack setup
3. stack build
4. stack install

The stack command installs a program cfg. Add the
installation dir to the PATH variable and try in the
root dir of this project the commands

1. cfg --help
2. cfg -l -g examples/Mini.cfg
