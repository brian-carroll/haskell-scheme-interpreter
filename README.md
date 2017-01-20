Haskell implementation of a simple Scheme compiler, based on [this tutorial] (https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).


---


Follow-on Projects
==================

I've finished the tutorial and it works! Yay, I built a language! :)

... What else can I do?


Comments
--------
- Scheme comments start with `;` and go to the end of the line
- Effectively they are treated the same as a space (delimiter between expressions)

To do:
- [x] Modify the `spaces` parser to become `spacesOrComments`
- [x] Deal with spaces at _start_ of input, which cause exceptions in the interpreter
    - [x] Parse `optional spacesOrComments` at start of input (in REPL and loaded files)
    - [x] For the REPL, use `Maybe` to deal with completely blank lines
        - This means I can hit Enter a few times without getting error messages back!
        - REPL only. When loading a file, you kinda _want_ an error if it's empty.


History
-------
- In the REPL, I'm finding it annoying not to be able to hit the up arrow! How to implement this?
- Need to read a character at a time rather than a line at a time.
    - Should it accept up arrow at start of line only, or any time?
- Store each entered expression in a history list
    - Add to the list each time an expression is evaluated
- Up and down arrows let you navigate the history list
    - How to detect these keys?
- Each time a character is entered, add it to the current input string
- If character is '\n' then add it to the history list and evaluate it
- How to store history list?
    - Arg to a recursive function?
    - Mutable variable?
- Found a package to do all of this! Haskeline is used in Elm REPL.

To do:
- [x] Install Haskeline
- [x] Set up basic version from docs example & make sure tests still pass
- [ ] Handle Ctrl-C like Elm REPL. If text has been entered, just start a new line. If no text, quit.

Tail Call Optimisation
----------------------
- Interesting. TCO kind of fascinates me for some reason.
- [x] Detect tail calls
- [x] Optimise tail calls
    - Can't easily do tail recursion for the `eval` function, because its 
        recursion needs to follow the structure of the Scheme code.
    - But can make `apply` tail recursive and use it as a [trampoline](https://en.wikipedia.org/wiki/Tail_call#Through_trampolining)
    - 3x reduction in memory usage but still eventually get stack overflow
- [ ] Completely eliminate stack overflow for recursive functions


Macro expansion
---------------
- Should I try to do this?
- Never used them, could be a good way to learn it.
- Could be a bit deep and confusing
