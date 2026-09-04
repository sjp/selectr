test_that("translation from parsed objects to XPath works", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    expect_equal(xpath("*"), "*")
    expect_equal(xpath("e"), "e")
    expect_equal(xpath("*|e"), "*[local-name() = 'e']")
    expect_equal(xpath("|e"), "e")
    expect_equal(xpath("e|f"), "e:f")
    expect_equal(xpath("e[foo]"), "e[@foo]")
    expect_equal(xpath("e[foo|bar]"), "e[@foo:bar]")
    expect_equal(xpath('e[foo="bar"]'), "e[@foo = 'bar']")
    expect_equal(xpath("e[foo='(test)']"), "e[@foo = '(test)']")
    expect_equal(xpath('e[foo="(test)"]'), "e[@foo = '(test)']")
    expect_equal(xpath("e[foo='(abc)']"), "e[@foo = '(abc)']")
    expect_equal(xpath("e[foo='(e2e)']"), "e[@foo = '(e2e)']")
    expect_equal(xpath('e[foo="(e2e)"]'), "e[@foo = '(e2e)']")
    expect_equal(xpath("e[foo='(123)']"), "e[@foo = '(123)']")
    expect_equal(xpath("e[foo='(12345)']"), "e[@foo = '(12345)']")
    # Six hex digits (max for CSS unicode escape)
    expect_equal(xpath("e[foo='(abcdef)']"), "e[@foo = '(abcdef)']")
    expect_equal(xpath("e[foo='(123456)']"), "e[@foo = '(123456)']")
    # Seven hex digits (exceeds max, so not unicode escape required)
    expect_equal(xpath("e[foo='(1234567)']"), "e[@foo = '(1234567)']")
    expect_equal(xpath("e[foo='(AbCdEf)']"), "e[@foo = '(AbCdEf)']")
    expect_equal(xpath("e[foo='(E2E)']"), "e[@foo = '(E2E)']")
    expect_equal(xpath("e[foo='(o2o)']"), "e[@foo = '(o2o)']")
    expect_equal(xpath('e[foo="(o2o)"]'), "e[@foo = '(o2o)']")
    expect_equal(xpath("e[foo='(xyz)']"), "e[@foo = '(xyz)']")
    expect_equal(xpath("e[foo='(test123)']"), "e[@foo = '(test123)']")
    expect_equal(xpath("e[foo='(abc)(def)']"), "e[@foo = '(abc)(def)']")
    expect_equal(xpath("e[foo='(abc )']"), "e[@foo = '(abc )']")
    # Unicode escapes are decoded to the characters they represent,
    # in idents, hashes, and strings alike
    expect_equal(xpath("#\\31 23"), "*[@id = '123']")
    expect_equal(xpath("\\31 23"),
                 "*[name() = '123' and namespace-uri() = '']")
    expect_equal(xpath("[\\31 23]"),
                 "*[attribute::*[name() = '123']]")
    expect_equal(xpath("e[foo='\\31 23']"), "e[@foo = '123']")
    expect_equal(xpath("e[foo='x\\79 z']"), "e[@foo = 'xyz']")
    expect_equal(xpath("e[foo='\\4a']"), "e[@foo = 'J']")
    # An escaped backslash yields a literal backslash; what follows it
    # must not be re-processed as another escape
    expect_equal(xpath("e[foo='x\\\\79 z']"), "e[@foo = 'x\\79 z']")
    expect_equal(xpath("e[foo='\\\\31 23']"), "e[@foo = '\\31 23']")
    expect_equal(xpath("#\\\\31 x"), "*[@id = '\\31']//x")
    expect_equal(xpath('e[foo~="bar"]'),
                 "e[contains(concat(' ', normalize-space(@foo), ' '), ' bar ')]")
    expect_equal(xpath('e[foo^="bar"]'),
                 "e[starts-with(@foo, 'bar')]")
    expect_equal(xpath('e[foo$="bar"]'),
                 "e[substring(@foo, string-length(@foo) - 2) = 'bar']")
    expect_equal(xpath('e[foo*="bar"]'),
                 "e[contains(@foo, 'bar')]")
    expect_equal(xpath('e[hreflang|="en"]'),
                 "e[@hreflang = 'en' or starts-with(@hreflang, 'en-')]")
    # CSS Selectors Level 4 case-sensitivity flags
    lower_foo <- paste0("translate(@foo, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',",
                        " 'abcdefghijklmnopqrstuvwxyz')")
    expect_equal(xpath('e[foo="Bar" i]'),
                 paste0("e[", lower_foo, " = 'bar']"))
    expect_equal(xpath('e[foo^="Bar" i]'),
                 paste0("e[starts-with(",
                        lower_foo, ", 'bar')]"))
    expect_equal(xpath('e[foo$="Bar" i]'),
                 paste0("e[substring(",
                        lower_foo, ", string-length(",
                        lower_foo, ") - 2) = 'bar']"))
    expect_equal(xpath('e[foo*="Bar" i]'),
                 paste0("e[contains(",
                        lower_foo, ", 'bar')]"))
    expect_equal(xpath('e[foo~="Bar" i]'),
                 paste0("e[",
                        "contains(concat(' ', normalize-space(",
                        lower_foo, "), ' '), ' bar ')]"))
    expect_equal(xpath('e[foo|="Bar" i]'),
                 paste0("e[",
                        lower_foo, " = 'bar' or starts-with(",
                        lower_foo, ", 'bar-')]"))
    # The 'i' flag is ASCII case-insensitive: non-ASCII characters such
    # as 'É' are left alone
    expect_equal(xpath("e[foo='\\C9 x' i]"),
                 paste0("e[", lower_foo, " = '\uC9x']"))
    # An empty value cannot differ by case, so it keeps the exact
    # (existence-preserving) translation
    expect_equal(xpath('e[foo="" i]'), "e[@foo = '']")
    # The 's' flag requests the default case-sensitive matching
    expect_equal(xpath('e[foo="Bar" s]'), "e[@foo = 'Bar']")
    expect_equal(xpath('e[foo^="Bar" s]'),
                 "e[starts-with(@foo, 'Bar')]")
    expect_equal(xpath('e:nth-child(1)'),
                 "e[count(preceding-sibling::*) = 0]")
    expect_equal(xpath('e:nth-child(3n+2)'),
                 "e[count(preceding-sibling::*) >= 1 and (count(preceding-sibling::*) + 2) mod 3 = 0]")
    expect_equal(xpath('e:nth-child(3n-2)'),
                 "e[count(preceding-sibling::*) mod 3 = 0]")
    expect_equal(xpath('e:nth-child(-n+6)'),
                 "e[count(preceding-sibling::*) <= 5]")
    expect_equal(xpath('e:nth-last-child(1)'),
                 "e[count(following-sibling::*) = 0]")
    expect_equal(xpath('e:nth-last-child(2n)'),
                 "e[(count(following-sibling::*) + 1) mod 2 = 0]")
    expect_equal(xpath('e:nth-last-child(2n+1)'),
                 "e[count(following-sibling::*) mod 2 = 0]")
    expect_equal(xpath('e:nth-last-child(2n+2)'),
                 "e[count(following-sibling::*) >= 1 and (count(following-sibling::*) + 1) mod 2 = 0]")
    expect_equal(xpath('e:nth-last-child(3n+1)'),
                 "e[count(following-sibling::*) mod 3 = 0]")
    expect_equal(xpath('e:nth-last-child(-n+2)'),
                 "e[count(following-sibling::*) <= 1]")
    expect_equal(xpath('e:nth-of-type(1)'),
                 "e[count(preceding-sibling::e) = 0]")
    expect_equal(xpath('e:nth-last-of-type(1)'),
                 "e[count(following-sibling::e) = 0]")
    expect_equal(xpath('div e:nth-last-of-type(1) .aclass'),
                 "div//e[count(following-sibling::e) = 0]//*[contains(concat(' ', normalize-space(@class), ' '), ' aclass ')]")
    expect_equal(xpath('e:first-child'),
                 "e[count(preceding-sibling::*) = 0]")
    expect_equal(xpath('e:last-child'),
                 "e[count(following-sibling::*) = 0]")
    expect_equal(xpath('e:first-of-type'),
                 "e[count(preceding-sibling::e) = 0]")
    expect_equal(xpath('e:last-of-type'),
                 "e[count(following-sibling::e) = 0]")
    expect_equal(xpath('e:only-child'),
                 "e[count(preceding-sibling::*) = 0 and count(following-sibling::*) = 0]")
    expect_equal(xpath('e:only-of-type'),
                 "e[count(preceding-sibling::e) = 0 and count(following-sibling::e) = 0]")
    # element names that cannot be used as an XPath name test still
    # support the of-type pseudo-classes via a name() node test, which
    # carries the null-namespace pin the name test would have implied
    expect_equal(xpath('é:first-of-type'),
                 "*[name() = 'é' and namespace-uri() = '' and count(preceding-sibling::*[name() = 'é' and namespace-uri() = '']) = 0]")
    expect_equal(xpath('é:last-of-type'),
                 "*[name() = 'é' and namespace-uri() = '' and count(following-sibling::*[name() = 'é' and namespace-uri() = '']) = 0]")
    expect_equal(xpath('é:only-of-type'),
                 "*[name() = 'é' and namespace-uri() = '' and count(preceding-sibling::*[name() = 'é' and namespace-uri() = '']) = 0 and count(following-sibling::*[name() = 'é' and namespace-uri() = '']) = 0]")
    expect_equal(xpath('é:nth-of-type(2)'),
                 "*[name() = 'é' and namespace-uri() = '' and count(preceding-sibling::*[name() = 'é' and namespace-uri() = '']) = 1]")
    expect_equal(xpath('é:nth-last-of-type(2)'),
                 "*[name() = 'é' and namespace-uri() = '' and count(following-sibling::*[name() = 'é' and namespace-uri() = '']) = 1]")
    # likewise for elements in any namespace, via local-name()
    expect_equal(xpath('*|e:first-of-type'),
                 "*[local-name() = 'e' and count(preceding-sibling::*[local-name() = 'e']) = 0]")
    expect_equal(xpath('e:empty'),
                 "e[not(*) and not(string-length())]")
    expect_equal(xpath('e:EmPTY'),
                 "e[not(*) and not(string-length())]")
    expect_equal(xpath('e:root'),
                 "e[not(parent::*)]")
    expect_equal(xpath('e:hover'),
                 "e[0]") #never matches
    expect_error(xpath('e:contains("foo")'),
                 "The pseudo-class :contains\\(\\) is unknown")
    expect_equal(xpath('e.warning'),
                 "e[contains(concat(' ', normalize-space(@class), ' '), ' warning ')]")
    expect_equal(xpath('e#myid'),
                 "e[@id = 'myid']")
    expect_equal(xpath('e:not(:nth-child(odd))'),
                 "e[not(count(preceding-sibling::*) mod 2 = 0)]")
    expect_equal(xpath('e:nOT(*)'),
                 "e[0]") # never matches
    # Selectors Level 4: :not() can nest inside functional pseudo-classes
    expect_equal(xpath(':not(:not(a))'),
                 "*[not(not(self::a))]")
    expect_equal(xpath('e:is(:not(f))'),
                 "e[not(self::f)]")
    expect_equal(xpath('e:has(:not(f))'),
                 "e[.//*[not(self::f)]]")
    # Selectors Level 4: complex selectors inside functional pseudo-classes
    # apply the rightmost compound to the candidate and walk the rest
    # through reversed axes
    expect_equal(xpath(':is(a b)'),
                 "*[self::b and ancestor::*[self::a]]")
    expect_equal(xpath(':is(a > b)'),
                 "*[self::b and parent::*[self::a]]")
    expect_equal(xpath(':is(a + b)'),
                 "*[self::b and preceding-sibling::*[1][self::a]]")
    expect_equal(xpath(':is(a ~ b)'),
                 "*[self::b and preceding-sibling::*[self::a]]")
    expect_equal(xpath(':is(a > b ~ c)'),
                 "*[self::c and preceding-sibling::*[self::b and parent::*[self::a]]]")
    # The combinator chain inside a functional pseudo-class parses the
    # same whether or not whitespace surrounds each combinator, and
    # whether or not trailing whitespace precedes the closing ')'/','
    expect_equal(xpath(':is(a>b>c)'), xpath(':is(a > b > c)'))
    expect_equal(xpath(':is(a > b )'), xpath(':is(a > b)'))
    expect_equal(xpath(':is(a, b )'), xpath(':is(a, b)'))
    # A nested or-group used as the rightmost compound of a combinator
    # chain, itself carrying no element name to AND against (so
    # add_name_test() leaves its condition_is_or flag untouched), is
    # parenthesized before being AND-ed with the reversed-axis test
    expect_equal(xpath(':is(a > :is(.x, .y))'),
                 paste0("*[(contains(concat(' ', normalize-space(@class), ' '), ' x ')",
                       " or contains(concat(' ', normalize-space(@class), ' '), ' y '))",
                       " and parent::*[self::a]]"))
    expect_equal(xpath('e:not(a b)'),
                 "e[not(self::b and ancestor::*[self::a])]")
    expect_equal(xpath(':where(a + b)'),
                 "*[self::b and preceding-sibling::*[1][self::a]]")
    expect_equal(xpath(':is(a.x > b#y)'),
                 "*[@id = 'y' and self::b and parent::*[contains(concat(' ', normalize-space(@class), ' '), ' x ') and self::a]]")
    # The :is()/:where() alternatives must stay grouped: conditions added
    # before or after the pseudo-class AND with the whole selector list,
    # rather than the ORs flattening into the compound's condition chain
    expect_equal(xpath('e.warning:is(.a, .b)'),
                 "e[contains(concat(' ', normalize-space(@class), ' '), ' warning ') and (contains(concat(' ', normalize-space(@class), ' '), ' a ') or contains(concat(' ', normalize-space(@class), ' '), ' b '))]")
    expect_equal(xpath(':is(f, g):first-child'),
                 "*[(self::f or self::g) and count(preceding-sibling::*) = 0]")
    expect_equal(xpath('e:is(.a):is(.b)'),
                 "e[contains(concat(' ', normalize-space(@class), ' '), ' a ') and contains(concat(' ', normalize-space(@class), ' '), ' b ')]")
    expect_equal(xpath('e.warning:where(f, g)'),
                 "e[contains(concat(' ', normalize-space(@class), ' '), ' warning ') and (self::f or self::g)]")
    # An always-true argument (a bare '*') must not be dropped from a
    # selector list: the whole list then matches everything, so :is()
    # imposes no condition, :not() never matches, and the "of S" form
    # counts all siblings
    # A forgiving selector list may be empty (selectors-4); with no
    # alternative to satisfy, ':is()' matches nothing
    expect_equal(xpath(':is()'), "*[0]")
    expect_equal(xpath(':matches()'), "*[0]")
    expect_equal(xpath('a:where()'), "a[0]")
    expect_equal(xpath(':is( )'), "*[0]")
    # ... and being always-false, it absorbs the rest of the compound:
    # a predicate with a constant-false conjunct is just "0"
    expect_equal(xpath('e.warning:is()'), "e[0]")
    expect_equal(xpath('e:is():nth-child(2)'), "e[0]")
    # ... and so, as an argument, it excludes nothing from a :not()
    expect_equal(xpath(':not(:is())'), "*[not(0)]")
    expect_equal(xpath(':is(:where())'), "*[0]")
    expect_equal(xpath(':is(f, *)'),
                 "*")
    expect_equal(xpath('e.warning:is(f, *)'),
                 "e[contains(concat(' ', normalize-space(@class), ' '), ' warning ')]")
    expect_equal(xpath('e:not(f, *)'),
                 "e[0]")
    expect_equal(xpath('e:nth-child(2 of f, *)'),
                 "e[count(preceding-sibling::*) = 1]")
    expect_equal(xpath('e:nth-last-child(2 of f, *)'),
                 "e[count(following-sibling::*) = 1]")
    expect_equal(xpath('e f'),
                 "e//f")
    expect_equal(xpath('e > f'),
                 "e/f")
    expect_equal(xpath('e + f'),
                 "e/following-sibling::*[1][self::f]")
    expect_equal(xpath('e ~ f'),
                 "e/following-sibling::f")
    expect_equal(xpath('e ~ f:nth-child(3)'),
                 "e/following-sibling::f[count(preceding-sibling::*) = 2]")
    expect_equal(xpath('div#container p'),
                 "div[@id = 'container']//p")

    # expect that the following do nothing for the generic translator
    expect_equal(xpath('a:any-link'), "a[0]")
    expect_equal(xpath('a:link'), "a[0]")
    expect_equal(xpath('a:visited'), "a[0]")
    expect_equal(xpath('a:hover'), "a[0]")
    expect_equal(xpath('a:active'), "a[0]")
    expect_equal(xpath('a:focus'), "a[0]")
    expect_equal(xpath('a:target'), "a[0]")
    expect_equal(xpath('a:target-within'), "a[0]")
    expect_equal(xpath('a:local-link'), "a[0]")
    expect_equal(xpath('a:enabled'), "a[0]")
    expect_equal(xpath('a:disabled'), "a[0]")
    expect_equal(xpath('a:checked'), "a[0]")

    # Invalid characters in XPath element names

    charsets <- localeToCharset()
    if (!anyNA(charsets) && charsets[1] == "UTF-8") {
        expect_equal(xpath('di\ua0v'),
                     "*[name() = 'di v' and namespace-uri() = '']") # div\ua0v
        expect_equal(xpath('[h\ua0ref]'),
                     "*[attribute::*[name() = 'h ref']]") # h\ua0ref
    }
    expect_equal(xpath('di\\[v'),
                 "*[name() = 'di[v' and namespace-uri() = '']")
    expect_equal(xpath('[h\\]ref]'),
                 "*[attribute::*[name() = 'h]ref']]")
})

test_that("an escaped delimiter names an element instead of being one", {
    gt <- GenericTranslator$new()
    xpath <- function(css, translator = gt) {
        translator$css_to_xpath(css, prefix = "")
    }

    # '\*' and '\2a' are <ident-token>s spelling the name '*', not the
    # universal selector: they select an element actually named '*'
    # (which no document has), and they count as a type selector
    for (css in c("\\*", "\\2a")) {
        expect_equal(xpath(css), "*[name() = '*' and namespace-uri() = '']")
        expect_equal(parse(css)[[1]]$specificity(), c(0, 0, 1))
    }
    expect_equal(xpath("*"), "*")

    # An escaped colon is part of the name, not the prefix separator of
    # a namespaced one: written into a node test, 'a\:b' would instead
    # ask the evaluator to resolve a namespace prefix 'a'
    expect_equal(xpath("a\\:b"), "*[name() = 'a:b' and namespace-uri() = '']")
    expect_equal(xpath("a\\:hover"),
                 "*[name() = 'a:hover' and namespace-uri() = '']")
    # An explicit prefix still resolves through the namespace map; only
    # the local name is quoted, colon and all
    expect_equal(xpath("svg|a\\:b"), "svg:*[local-name() = 'a:b']")

    # The quoted name is also what a sibling combinator and an of-type
    # pseudo-class match against
    expect_equal(xpath("a\\:b + c"),
                 paste0("*[name() = 'a:b' and namespace-uri() = '']",
                        "/following-sibling::*[1][self::c]"))
    expect_equal(xpath("a\\:b:first-of-type"),
                 paste0("*[name() = 'a:b' and namespace-uri() = '' and ",
                        "count(preceding-sibling::*[name() = 'a:b' and ",
                        "namespace-uri() = '']) = 0]"))

    # The HTML translator lowercases such a name like any other
    expect_equal(xpath("A\\:B", HTMLTranslator$new()),
                 "*[name() = 'a:b' and namespace-uri() = '']")
})

test_that("an always-false condition absorbs the rest of the compound", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    # Whichever simple selector contributes the "0", and whichever side
    # of the compound it is written on, the predicate is just "[0]": a
    # conjunct that is constant-false decides the whole predicate, and
    # the conditions AND-ed with it only obscure that
    expect_equal(xpath("e.warning:is()"), "e[0]")
    expect_equal(xpath("e:is().warning"), "e[0]")
    expect_equal(xpath("e.warning:not(*)"), "e[0]")
    expect_equal(xpath("e.warning:dir(ltr)"), "e[0]")
    expect_equal(xpath("e.warning[href^='']"), "e[0]")
    expect_equal(xpath("e.warning:nth-child(0)"), "e[0]")
    expect_equal(xpath("e:nth-child(-2n-1 of .item)"), "e[0]")
    # ... including when the compound is a functional pseudo-class
    # argument, where the reversed-axis test of a complex argument goes
    # the same way
    expect_equal(xpath("e:is(.a:is())"), "e[0]")
    expect_equal(xpath("e:is(f > g:is())"), "e[0]")
    # The fold is a simplification of the expression, not of what it
    # selects: an impossible compound still contributes its step to the
    # path, and a negated one is still always true
    expect_equal(xpath("e:is() f"), "e[0]//f")
    expect_equal(xpath("e:not(:is())"), "e[not(0)]")
})

test_that("comments adjacent to whitespace translate like their comment-free equivalents", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    expect_equal(xpath("a /* c */ /* d */ b"), xpath("a b"))
    expect_equal(xpath(":is(a /*x*/ /*y*/ b)"), xpath(":is(a b)"))
    expect_equal(xpath("[a /*x*/ = /*y*/ 'b']"), xpath("[a = 'b']"))
    expect_equal(xpath("a /*x*/ , b"), xpath("a, b"))
    expect_equal(xpath("a /*x*/ /*y*/, b"), xpath("a, b"))
})

test_that("invalid unicode escapes translate to U+FFFD", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }
    repl <- "\uFFFD"

    # Null, surrogate and out-of-range escapes are replacement
    # characters, not errors (css-syntax-3)
    for (esc in c("\\0", "\\D800", "\\DFFF", "\\110000", "\\FFFFFF")) {
        expect_equal(xpath(esc),
                     paste0("*[name() = '", repl,
                            "' and namespace-uri() = '']"))
        expect_equal(xpath(paste0("#", esc)),
                     paste0("*[@id = '", repl, "']"))
        expect_equal(xpath(paste0("[x=\"", esc, "\"]")),
                     paste0("*[@x = '", repl, "']"))
        expect_equal(xpath(paste0(".", esc)),
                     paste0("*[contains(concat(' ', ",
                            "normalize-space(@class), ' '), ' ",
                            repl, " ')]"))
    }

    # No escape whatsoever throws anything but a parse error
    for (hex in c("0", "1", "D7FF", "D800", "DC00", "DFFF", "E000",
                  "10FFFF", "110000", "FFFFFF", "ffffff")) {
        for (css in c(paste0("\\", hex), paste0("a.\\", hex),
                      paste0("[x=\"\\", hex, "\"]"))) {
            expect_true(is.character(tryCatch(xpath(css),
                                              selectr_parse_error = function(e) "")))
        }
    }
})

test_that("long combinator chains translate without recursion limits", {
    # Translation walks the left-deep parse tree in a loop, so the
    # length of a chain is bounded by memory rather than by R's
    # expression nesting limit (options(expressions=))
    n <- 2000
    chain <- function(combinator) paste(rep("a", n), collapse = combinator)
    repeated <- function(step) paste0("descendant-or-self::a",
                                      paste(rep(step, n - 1), collapse = ""))

    expect_equal(css_to_xpath(chain(" > ")), repeated("/a"))
    expect_equal(css_to_xpath(chain(" ")), repeated("//a"))
    expect_equal(css_to_xpath(chain(" ~ ")),
                 repeated("/following-sibling::a"))
    expect_equal(css_to_xpath(chain(" + ")),
                 repeated("/following-sibling::*[1][self::a]"))
})

test_that("long combinator chains report and score without recursion", {
    n <- 2000
    selectors <- parse(paste(rep("a", n), collapse = " > "))
    expect_equal(length(selectors), 1)

    # repr() and specificity() walk the same left spine iteratively
    expect_equal(selectors[[1]]$specificity(), c(0, 0, n))
    expect_true(grepl("^CombinedSelector\\[", selectors[[1]]$repr()))

    # Pseudo-class arguments are still translated recursively; this is
    # a regression guard on a nesting depth that is known to work
    nested <- paste0(paste(rep(":not(", 250), collapse = ""), "a",
                     paste(rep(")", 250), collapse = ""))
    expect_true(is.character(css_to_xpath(nested)))
})
