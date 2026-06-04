context("tokenizer")

test_that("tokenizer extracts correct representation", {
    tokens <- tokenize('E > f [a~="y\\"x"]:nth(/* fu /]* */-3.7)')
    tokens <- unlist(lapply(tokens, function(x) x$repr()))
    expected_tokens <- c("<IDENT 'E' at 1>",
                         "<S ' ' at 2>",
                         "<DELIM '>' at 3>",
                         "<S ' ' at 4>",
                         "<IDENT 'f' at 5>",
                         "<S ' ' at 6>",
                         "<DELIM '[' at 7>",
                         "<IDENT 'a' at 8>",
                         "<DELIM '~=' at 9>",
                         "<STRING 'y\"x' at 11>",
                         "<DELIM ']' at 17>",
                         "<DELIM ':' at 18>",
                         "<IDENT 'nth' at 19>",
                         "<DELIM '(' at 22>",
                         "<NUMBER '-3.7' at 35>",
                         "<DELIM ')' at 39>",
                         "<EOF at 40>")
    expect_that(tokens, equals(expected_tokens))
})

test_that("unicode escapes are decoded in idents, hashes, and strings", {
    reprs <- function(css) {
        unlist(lapply(tokenize(css), function(x) x$repr()))
    }

    # '\31 ' is U+0031, i.e. '1' -- the only way to write an ID
    # beginning with a digit
    expect_that(reprs("#\\31 23"),
                equals(c("<HASH '123' at 1>", "<EOF at 8>")))
    expect_that(reprs("\\31 23"),
                equals(c("<IDENT '123' at 1>", "<EOF at 7>")))
    expect_that(reprs("x\\79 z"),
                equals(c("<IDENT 'xyz' at 1>", "<EOF at 7>")))
    # Hex digits in escapes are case-insensitive
    expect_that(reprs("'\\4a b'"),
                equals(c("<STRING 'Jb' at 1>", "<EOF at 8>")))
    expect_that(reprs("'\\4A b'"),
                equals(c("<STRING 'Jb' at 1>", "<EOF at 8>")))
    # A whitespace terminator is consumed even after six hex digits
    expect_that(reprs("'\\00004a b'"),
                equals(c("<STRING 'Jb' at 1>", "<EOF at 12>")))
    # Simple escapes of delimiters still work
    expect_that(reprs("di\\[v"),
                equals(c("<IDENT 'di[v' at 1>", "<EOF at 6>")))
    expect_that(reprs("#a\\[b"),
                equals(c("<HASH 'a[b' at 1>", "<EOF at 6>")))
})
