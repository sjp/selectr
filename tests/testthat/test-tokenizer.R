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
