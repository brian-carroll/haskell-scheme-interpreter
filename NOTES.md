COMMENTS
--------
Comments should be treated like spaces. They are separators between expressions.
Easiest way to handle them is to build them into the definition of 'whitespace'.


However, at the moment, the parser uses spaces to mark the END of an expression.
But it freaks out if it sees spaces BEFORE an expression.

- Comment/space AFTER an expression
    - Ignored safely

- Comment/space as FIRST input
    - Generates exceptions
        - REPL: Parse error `unexpected end of input`
        - Loading a file: Haskell function `last` gets applied to an empty list
    - Where do we want this to work?
        - Blank lines in the REPL
        - Comments at top of file

- Create a new LispVal constructor?
    - BlankStartOfInput
    - `show` function should generate ""
    - Evaluates as itself, just like `Number`, `String`, etc.
    - Will only ever be parsed at start of input, so won't mess up calculations.
        - Ensure this by making it the lowest priority in the parser
        - This means other spaces/comments will still be considered to be AFTER expressions,
            and processed the same way they are now, which is fine.

- Make a dedicated action to eat initial spaces
    - Make sure it's only done when it needs to be
    - REPL
        - Ideally what I'd like to do is:
            - Eat spaces & comments
            - Parse either `eof` or an expression
    - Load file
        - Eval does all the work, in the special `load` clause
        - Ideally what I'd like to do is:
            - Eat spaces & comments
            - Process rest of file as before

        ```
        readExprOrBlank =
            do
                try spaces
                (eof <|> readExpr)
        ```

- All uses of `readExpr`
    - `read` primitive (Primitives.hs)
        - Reads and interprets line-by-line.
        - Can't even handle multi-line Scheme forms
        - OK/good to make it ignore initial comments or blanks.
    - `evalString` in REPL (Main.hs)
        - OK/good to make it ignore initial comments or blanks.

- All uses of `readExprList`
    - `parseFile` (Primitives.hs)
        - `load` primitive
        - OK/good to make it ignore initial comments or blanks.
