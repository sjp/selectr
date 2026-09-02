test_that("tokenizer extracts correct representation", {
    tokens <- tokenize('E > f [a~="y\\"x"]:nth(/* fu /]* */-3.7)')
    tokens <- unlist(lapply(tokens, token_repr))
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
    expect_equal(tokens, expected_tokens)
})

test_that("unicode escapes are decoded in idents, hashes, and strings", {
    reprs <- function(css) {
        unlist(lapply(tokenize(css), token_repr))
    }

    # '\31 ' is U+0031, i.e. '1' -- the only way to write an ID
    # beginning with a digit
    expect_equal(reprs("#\\31 23"),
                 c("<HASH '123' at 1>", "<EOF at 8>"))
    expect_equal(reprs("\\31 23"),
                 c("<IDENT '123' at 1>", "<EOF at 7>"))
    expect_equal(reprs("x\\79 z"),
                 c("<IDENT 'xyz' at 1>", "<EOF at 7>"))
    # Hex digits in escapes are case-insensitive
    expect_equal(reprs("'\\4a b'"),
                 c("<STRING 'Jb' at 1>", "<EOF at 8>"))
    expect_equal(reprs("'\\4A b'"),
                 c("<STRING 'Jb' at 1>", "<EOF at 8>"))
    # A whitespace terminator is consumed even after six hex digits
    expect_equal(reprs("'\\00004a b'"),
                 c("<STRING 'Jb' at 1>", "<EOF at 12>"))
    # Simple escapes of delimiters still work
    expect_equal(reprs("di\\[v"),
                 c("<IDENT 'di[v' at 1>", "<EOF at 6>"))
    expect_equal(reprs("#a\\[b"),
                 c("<HASH 'a[b' at 1>", "<EOF at 6>"))
})

test_that("invalid unicode escapes decode to U+FFFD", {
    reprs <- function(css) {
        unlist(lapply(tokenize(css), token_repr))
    }
    repl <- "\uFFFD"

    # css-syntax-3: null, surrogate and out-of-range code points all
    # decode to the replacement character rather than failing
    for (esc in c("\\0", "\\000000", "\\D800", "\\d800", "\\DFFF",
                  "\\110000", "\\FFFFFF")) {
        expect_equal(reprs(esc),
                     c(paste0("<IDENT '", repl, "' at 1>"),
                       paste0("<EOF at ", nchar(esc) + 1, ">")))
    }
    # ... and in the middle of a token, in each token type
    expect_equal(reprs("a\\0 b"),
                 c(paste0("<IDENT 'a", repl, "b' at 1>"), "<EOF at 6>"))
    # (the terminating space keeps 'b' out of the escape's hex digits)
    expect_equal(reprs("#a\\D800 b"),
                 c(paste0("<HASH 'a", repl, "b' at 1>"), "<EOF at 10>"))
    expect_equal(reprs("'a\\110000b'"),
                 c(paste0("<STRING 'a", repl, "b' at 1>"), "<EOF at 12>"))
    # The last code point that is not a surrogate, and the first after
    # them, are still decoded normally
    expect_equal(reprs("\\D7FF"),
                 c("<IDENT '\uD7FF' at 1>", "<EOF at 6>"))
    expect_equal(reprs("\\E000"),
                 c("<IDENT '\uE000' at 1>", "<EOF at 6>"))
    expect_equal(reprs("\\10FFFF"),
                 c("<IDENT '\U0010FFFF' at 1>", "<EOF at 8>"))
})

test_that("a trailing backslash at EOF decodes to U+FFFD", {
    reprs <- function(css) {
        unlist(lapply(tokenize(css), token_repr))
    }
    repl <- "\uFFFD"

    # css-syntax-3 "consume an escaped code point": EOF right after the
    # backslash is still a valid escape, decoding to U+FFFD, in an
    # ident or a hash name
    expect_equal(reprs("\\"),
                 c(paste0("<IDENT '", repl, "' at 1>"), "<EOF at 2>"))
    expect_equal(reprs("a\\"),
                 c(paste0("<IDENT 'a", repl, "' at 1>"), "<EOF at 3>"))
    expect_equal(reprs("#a\\"),
                 c(paste0("<HASH 'a", repl, "' at 1>"), "<EOF at 4>"))

    # "consume a string token" special-cases this instead: a backslash
    # with nothing after it does nothing, so it is simply dropped, and
    # the still-open string is auto-closed as usual
    expect_equal(reprs("'a\\"),
                 c("<STRING 'a' at 1>", "<EOF at 4>"))
})

test_that("string tokens handle quotes, escapes, and unclosed strings", {
    reprs <- function(css) {
        unlist(lapply(tokenize(css), token_repr))
    }

    expect_equal(reprs("''"),
                 c("<STRING '' at 1>", "<EOF at 3>"))
    expect_equal(reprs("'a''b'"),
                 c("<STRING 'a' at 1>", "<STRING 'b' at 4>",
                   "<EOF at 7>"))
    # The other quote character is just content
    expect_equal(reprs("'\"'"),
                 c("<STRING '\"' at 1>", "<EOF at 4>"))
    # Escaped quotes do not close the string
    expect_equal(reprs("'a\\'b'"),
                 c("<STRING 'a'b' at 1>", "<EOF at 7>"))
    # An escaped backslash does not escape a following quote
    expect_equal(reprs("'a\\\\'"),
                 c("<STRING 'a\\' at 1>", "<EOF at 6>"))

    # A string still open at EOF is auto-closed with its consumed
    # value (css-syntax), including when the consumed value ends with
    # an escaped quote
    expect_equal(reprs("'abc"),
                 c("<STRING 'abc' at 1>", "<EOF at 5>"))
    expect_equal(reprs("a'"),
                 c("<IDENT 'a' at 1>", "<STRING '' at 2>",
                   "<EOF at 3>"))
    expect_equal(reprs("'a\\'"),
                 c("<STRING 'a'' at 1>", "<EOF at 5>"))
    # A raw newline may not appear in a string, and stops it short of
    # EOF, so it is not auto-closed
    # tokenize() is called directly here, bypassing the position/gutter
    # enrichment parse() applies at the css_to_xpath() boundary (see
    # "position is reported once, at the parse() boundary" below)
    expect_error(tokenize("'a\nb'"), "^Unclosed string$")
    expect_error(tokenize("'a\n"), "^Unclosed string$")
})

test_that("tokens are unaffected by where the match window falls", {
    reprs <- function(css) {
        unlist(lapply(tokenize(css), token_repr))
    }

    # tokenize() matches against a bounded window of the input rather
    # than the whole remaining tail, so a token can straddle the
    # window's end. Slide each construct across the boundary one
    # character at a time -- behind a run of 'a's and a space, so the
    # padding is always exactly two tokens -- and check that it still
    # comes out whole.
    cases <- list(
        # tokens longer than the window itself
        list("abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnop",
             "IDENT 'abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnop'"),
        list("1234567890123456789012345678901234567890",
             "NUMBER '1234567890123456789012345678901234567890'"),
        # numbers, whose fractional part is what a window can cut off
        list("12345.6789", "NUMBER '12345.6789'"),
        list(".5", "NUMBER '.5'"),
        list("-.25", "NUMBER '-.25'"),
        list("+7", "NUMBER '+7'"),
        # multi-character escapes, which a window can split anywhere
        list("\\41 z", "IDENT 'Az'"),
        list("\\1F600 z", "IDENT '\U0001F600z'"),
        list("\\\\z", "IDENT '\\z'"),
        list("#\\31 abc", "HASH '1abc'"),
        # strings scan for their own closing quote
        list("'abcdefghijklmnopqrstuvwxyz0123456789'",
             "STRING 'abcdefghijklmnopqrstuvwxyz0123456789'"),
        list("'a\\'b'", "STRING 'a'b'"))

    for (case in cases) {
        for (pad in 1:80) {
            css <- paste0(strrep("a", pad), " ", case[[1]])
            expect_equal(reprs(css),
                         c(paste0("<IDENT '", strrep("a", pad),
                                  "' at 1>"),
                           paste0("<S ' ' at ", pad + 1, ">"),
                           paste0("<", case[[2]], " at ", pad + 2, ">"),
                           paste0("<EOF at ", nchar(css) + 1, ">")))
        }
    }

    # A comment is skipped wherever the window boundary lands in it,
    # including when it runs unterminated to the end of the input
    for (pad in 1:80) {
        prefix <- paste0(strrep("a", pad), " ")
        head <- c(paste0("<IDENT '", strrep("a", pad), "' at 1>"),
                  paste0("<S ' ' at ", pad + 1, ">"))
        expect_equal(reprs(paste0(prefix, "/* comment body */b")),
                     c(head,
                       paste0("<IDENT 'b' at ", pad + 20, ">"),
                       paste0("<EOF at ", pad + 21, ">")))
        expect_equal(reprs(paste0(prefix, "/* unterminated")),
                     c(head, paste0("<EOF at ", pad + 17, ">")))
    }
})

test_that("consecutive whitespace runs collapse into a single S token", {
    reprs <- function(css) {
        unlist(lapply(tokenize(css), token_repr))
    }

    # A comment leaves no token behind, so whitespace on either side of
    # it -- or of a run of comments -- must not produce two adjacent S
    # tokens; every downstream consumer assumes at most one.
    expect_equal(reprs(" /*x*/ /*y*/ a"),
                 c("<S ' ' at 1>", "<IDENT 'a' at 14>", "<EOF at 15>"))
    expect_equal(tokenize(" /*x*/ /*y*/ a")[[1]]$type, "S")
    expect_equal(sum(vapply(tokenize(" /*x*/ /*y*/ a"), function(t) t$type,
                             character(1)) == "S"),
                 1)

    # The merged token keeps the position of the first whitespace run,
    # so an error caret still points at its start
    expect_equal(reprs("a /*x*/ /*y*/ ,b"),
                 c("<IDENT 'a' at 1>", "<S ' ' at 2>", "<DELIM ',' at 15>",
                   "<IDENT 'b' at 16>", "<EOF at 17>"))

    # A comment longer than the tokenizer's match window (64 chars)
    # still collapses whitespace on either side of it correctly
    long_comment <- paste0("/*", strrep("x", 100), "*/")
    expect_equal(reprs(paste0("a ", long_comment, " b")),
                 c("<IDENT 'a' at 1>",
                   paste0("<S ' ' at 2>"),
                   paste0("<IDENT 'b' at ", 4 + nchar(long_comment), ">"),
                   paste0("<EOF at ", 5 + nchar(long_comment), ">")))
})

test_that("a very long selector tokenizes in linear time", {
    # tokenize() used to slice off the whole remaining input at every
    # position, which made it quadratic in the selector's length. This
    # is a smoke test rather than a timing one: at 50 000 characters
    # the old implementation took seconds, so a regression here is felt
    # rather than asserted.
    css <- paste(rep("a.b", 12500), collapse = " ")
    expect_equal(nchar(css), 49999)
    tokens <- tokenize(css)
    # IDENT '.' IDENT per repeat, an S between each pair, plus EOF
    expect_equal(length(tokens), 3 * 12500 + 12499 + 1)
    expect_equal(token_repr(tokens[[1]]), "<IDENT 'a' at 1>")
    expect_equal(token_repr(tokens[[length(tokens)]]),
                 paste0("<EOF at ", nchar(css) + 1, ">"))
})
