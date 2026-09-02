test_that("parser parses canonical test expressions", {
    parse_many <- function(css) {
        selectors <- lapply(css, function(x) parse(x))
        n <- length(selectors)
        results <- list()
        for (i in seq_len(n)) {
            selector <- selectors[[i]]
            if (is.list(selector)) {
                results[[i]] <- unlist(lapply(selector, function(x) x$repr()))
            } else {
                results[[i]] <- selector$repr()
            }
        }
        if (n)
            unlist(results)
        else
            character(0)
    }

    expect_equal(parse_many("*"), "Element[*]")
    expect_equal(parse_many("*|*"), "Element[*|*]")
    expect_equal(parse_many("*|foo"), "Element[*|foo]")
    expect_equal(parse_many("|foo"), "Element[|foo]")
    expect_equal(parse_many("foo|*"), "Element[foo|*]")
    expect_equal(parse_many("foo|bar"), "Element[foo|bar]")
    expect_equal(parse_many('foo[lang|="zh"]'), "Attrib[Element[foo][lang |= 'zh']]")
    # This will never match, but it is valid:
    expect_equal(parse_many("#foo#bar"),
                 "Hash[Hash[Element[*]#foo]#bar]")
    expect_equal(parse_many(c("div>.foo",
                              "div> .foo",
                              "div >.foo",
                              "div > .foo",
                              "div > .foo",
                              "div \n>  \t \t .foo",
                              "div\r>\n\n\n.foo",
                              "div\f>\f.foo")),
                 rep("CombinedSelector[Element[div] > Class[Element[*].foo]]", 8))
    expect_equal(parse_many(c("td.foo,.bar",
                              "td.foo, .bar",
                              "td.foo\t\r\n\f ,\t\r\n\f .bar")),
                 rep(c("Class[Element[td].foo]",
                       "Class[Element[*].bar]"), 3))
    expect_equal(parse_many(c("div, td.foo, div.bar span")),
                 c("Element[div]",
                   "Class[Element[td].foo]",
                   "CombinedSelector[Class[Element[div].bar] <followed> Element[span]]"))
    expect_equal(parse_many("div > p"),
                 "CombinedSelector[Element[div] > Element[p]]")
    expect_equal(parse_many("td:first"),
                 "Pseudo[Element[td]:first]")
    expect_equal(parse_many("td :first"),
                 "CombinedSelector[Element[td] <followed> Pseudo[Element[*]:first]]")
    expect_equal(parse_many(c("a[name]", "a[ name\t]")),
                 rep("Attrib[Element[a][name]]", 2))
    expect_equal(parse_many("a [name]"),
                 "CombinedSelector[Element[a] <followed> Attrib[Element[*][name]]]")
    expect_equal(parse_many(c('a[rel="include"]', 'a[rel = include]')),
                 rep("Attrib[Element[a][rel = 'include']]", 2))
    expect_equal(parse_many(c("a[hreflang |= 'en']", "a[hreflang|=en]")),
                 rep("Attrib[Element[a][hreflang |= 'en']]", 2))
    expect_equal(parse_many(c('a[rel="include" i]', "a[rel = include I]",
                              'a[rel="include"i]')),
                 rep("Attrib[Element[a][rel = 'include' i]]", 3))
    expect_equal(parse_many(c('a[rel="include" s]', "a[rel = include S]")),
                 rep("Attrib[Element[a][rel = 'include' s]]", 2))
    # 'i' and 's' are only flags in the flag position, not as values
    expect_equal(parse_many("a[rel=i]"),
                 "Attrib[Element[a][rel = 'i']]")
    expect_equal(parse_many('a[rel="s" i]'),
                 "Attrib[Element[a][rel = 's' i]]")
    expect_equal(parse_many("div:nth-child(10)"),
                 "Function[Element[div]:nth-child(['10'])]")
    expect_equal(parse_many(":nth-child(2n+2)"),
                 "Function[Element[*]:nth-child(['2', 'n', '+2'])]")
    expect_equal(parse_many("div:nth-of-type(10)"),
                 "Function[Element[div]:nth-of-type(['10'])]")
    expect_equal(parse_many("div div:nth-of-type(10) .aclass"),
                 "CombinedSelector[CombinedSelector[Element[div] <followed> Function[Element[div]:nth-of-type(['10'])]] <followed> Class[Element[*].aclass]]")
    expect_equal(parse_many("label:only"),
                 "Pseudo[Element[label]:only]")
    expect_equal(parse_many("a:lang(fr)"),
                 "Function[Element[a]:lang(['fr'])]")
    expect_equal(parse_many('div:lang("foo")'),
                 "Function[Element[div]:lang(['foo'])]")
    expect_equal(parse_many("div#foobar"),
                 "Hash[Element[div]#foobar]")
    expect_equal(parse_many("div:not(div.foo)"),
                 "Negation[Element[div]:not(Class[Element[div].foo])]")

    # :not() with multiple arguments
    expect_equal(parse_many("div:not(.foo, .bar)"),
                 "Negation[Element[div]:not(Class[Element[*].foo], Class[Element[*].bar])]")
    expect_equal(parse_many("p:not(.foo, #bar)"),
                 "Negation[Element[p]:not(Class[Element[*].foo], Hash[Element[*]#bar])]")
    expect_equal(parse_many(":not(p, span, div)"),
                 "Negation[Element[*]:not(Element[p], Element[span], Element[div])]")
    expect_equal(parse_many("div:not([disabled], .hidden)"),
                 "Negation[Element[div]:not(Attrib[Element[*][disabled]], Class[Element[*].hidden])]")
    expect_equal(parse_many(":not(:hover, :visited, :active)"),
                 "Negation[Element[*]:not(Pseudo[Element[*]:hover], Pseudo[Element[*]:visited], Pseudo[Element[*]:active])]")
    expect_equal(parse_many("a:not(.link, [href], #special)"),
                 "Negation[Element[a]:not(Class[Element[*].link], Attrib[Element[*][href]], Hash[Element[*]#special])]")

    expect_equal(parse_many(":not(:not(a))"),
                 "Negation[Element[*]:not(Negation[Element[*]:not(Element[a])])]")
    expect_equal(parse_many("div:is(:not(.foo))"),
                 "Matching[Element[div]:is(Negation[Element[*]:not(Class[Element[*].foo])])]")

    expect_equal(parse_many("div:is(.foo, #bar)"),
                 "Matching[Element[div]:is(Class[Element[*].foo], Hash[Element[*]#bar])]")
    expect_equal(parse_many(":is(:hover, :visited)"),
                 "Matching[Element[*]:is(Pseudo[Element[*]:hover], Pseudo[Element[*]:visited])]")
    expect_equal(parse_many("div:matches(.foo, #bar)"),
                 "Matching[Element[div]:is(Class[Element[*].foo], Hash[Element[*]#bar])]")
    expect_equal(parse_many(":matches(:hover, :visited)"),
                 "Matching[Element[*]:is(Pseudo[Element[*]:hover], Pseudo[Element[*]:visited])]")

    expect_equal(parse_many("div:where(.foo, #bar)"),
                 "Where[Element[div]:where(Class[Element[*].foo], Hash[Element[*]#bar])]")
    expect_equal(parse_many(":where(:hover, :visited)"),
                 "Where[Element[*]:where(Pseudo[Element[*]:hover], Pseudo[Element[*]:visited])]")

    expect_equal(parse_many("div:has(.foo)"),
                 "Has[Element[div]:has(Class[Element[*].foo])]")
    expect_equal(parse_many("ul:has(li)"),
                 "Has[Element[ul]:has(Element[li])]")
    expect_equal(parse_many(":has(p, div)"),
                 "Has[Element[*]:has(Element[p], Element[div])]")

    # :has() with leading combinators (selectors-4 relative selectors)
    expect_equal(parse_many("e:has(> img)"),
                 "Has[Element[e]:has(RelativeSelector[> Element[img]])]")
    expect_equal(parse_many("e:has(~ p)"),
                 "Has[Element[e]:has(RelativeSelector[~ Element[p]])]")
    expect_equal(parse_many("e:has(+ p)"),
                 "Has[Element[e]:has(RelativeSelector[+ Element[p]])]")
    expect_equal(parse_many("e:has(> a, ~ .foo, p)"),
                 "Has[Element[e]:has(RelativeSelector[> Element[a]], RelativeSelector[~ Class[Element[*].foo]], Element[p])]")

    # complex selectors inside functional pseudo-classes (selectors-4)
    expect_equal(parse_many(":is(a b)"),
                 "Matching[Element[*]:is(CombinedSelector[Element[a] <followed> Element[b]])]")
    expect_equal(parse_many(":not(a > b)"),
                 "Negation[Element[*]:not(CombinedSelector[Element[a] > Element[b]])]")
    expect_equal(parse_many(":where(a + b, c)"),
                 "Where[Element[*]:where(CombinedSelector[Element[a] + Element[b]], Element[c])]")
    expect_equal(parse_many("e:has(> a b.x)"),
                 "Has[Element[e]:has(RelativeSelector[> CombinedSelector[Element[a] <followed> Class[Element[b].x]]])]")

    expect_equal(parse_many("td ~ th"),
                 "CombinedSelector[Element[td] ~ Element[th]]")

    # handle comments
    expect_equal(parse_many("a /* test */"),
                 "Element[a]")
    expect_equal(parse_many("a/* test */"),
                 "Element[a]")
    expect_equal(parse_many("/* test */ a"),
                 "Element[a]")
    expect_equal(parse_many("/* test */a"),
                 "Element[a]")
    expect_equal(parse_many("a /* test */ b"),
                 "CombinedSelector[Element[a] <followed> Element[b]]")
    expect_equal(parse_many("a /* test "),
                 "Element[a]")

    # comments surrounded by whitespace, or separated only by
    # whitespace, used to leave two adjacent S tokens behind and break
    # parsing wherever a following S was read as a combinator
    expect_equal(parse_many("a /* c */ /* d */ b"),
                 "CombinedSelector[Element[a] <followed> Element[b]]")
    expect_equal(parse_many(":is(a /*x*/ /*y*/ b)"),
                 "Matching[Element[*]:is(CombinedSelector[Element[a] <followed> Element[b]])]")
    expect_equal(parse_many("[a /*x*/ = /*y*/ 'b']"),
                 "Attrib[Element[*][a = 'b']]")
    expect_equal(parse_many("a /*x*/ , b"),
                 c("Element[a]", "Element[b]"))
    expect_equal(parse_many("a /*x*/ /*y*/, b"),
                 c("Element[a]", "Element[b]"))
})

test_that("parsed elements print correctly", {
    shw <- function(x) trimws(capture.output(parse(x)[[1]]$show()))

    expect_equal(shw("a"), "Element[a]")
    expect_equal(shw(".test"), "Class[Element[*].test]")
    expect_equal(shw(":active"), "Pseudo[Element[*]:active]")
    expect_equal(shw("a:not(.toggle)"), "Negation[Element[a]:not(Class[Element[*].toggle])]")

    # :not() with multiple arguments print tests
    expect_equal(shw("div:not(.foo, .bar)"),
                 "Negation[Element[div]:not(Class[Element[*].foo], Class[Element[*].bar])]")
    expect_equal(shw("p:not(span, div, a)"),
                 "Negation[Element[p]:not(Element[span], Element[div], Element[a])]")

    expect_equal(shw("[href]"), "Attrib[Element[*][href]]")
    expect_equal(shw("#id"), "Hash[Element[*]#id]")
})

test_that("compiled regex parsing functions behave as expected", {
    m_whitespace <- compile_('^[ \t\r\n\f]+')
    m_number <- compile_('^[+-]?(?:[0-9]*\\.[0-9]+|[0-9]+)')
    m_hash <- compile_(paste0("^#([_a-zA-Z0-9-]|", nonascii, "|\\\\(?:", delim_escapes, "))+"))
    m_ident <- compile_(paste0("^([_a-zA-Z0-9-]|", nonascii, "|\\\\(?:", delim_escapes, "))+"))

    expect_equal(m_whitespace("a b"), match_whitespace("a b"))
    expect_equal(m_whitespace(" a b"), match_whitespace(" a b"))
    expect_equal(m_number("a 1"), match_number("a 1"))
    expect_equal(m_number("1 a"), match_number("1 a"))
    expect_equal(m_hash("a #test"), match_hash("a #test"))
    expect_equal(m_ident(" test"), match_ident(" test"))
})

test_that("fast-path parses agree with the full parser", {
    full_parse <- function(css) {
        stream <- TokenStream$new(tokenize(css))
        parse_selector_group(stream)
    }
    reprs <- function(selectors) {
        unlist(lapply(selectors, function(s) s$repr()))
    }

    selectors <- c(
        # element fast path (h1 previously missed it: letters only)
        "div", "h1", " div ", "x-tag", "a_b",
        # id fast path; ids may lead with '_' or '-' but not a digit
        "#bar", "foo#bar", "#_x", "#-x", "#--x", "h1#a-1", " #x ",
        # class fast path (dead before: indexed out of bounds)
        ".foo", "foo.bar", "h2.a_b", " .foo ",
        # near misses that must fall through to the full parser
        "*", "a b", "a.b.c", "a:hover", "é", ".é", "-x", "#a.b",
        # an escaped digit is a legal id, but only via the full parser
        "#\\31 23")
    for (css in selectors) {
        expect_equal(reprs(parse(css)), reprs(full_parse(css)), info = css)
    }
})

test_that("token_equality always returns a single logical", {
    ident <- Token("IDENT", "a", 1)
    eof <- EOFToken(2)

    expect_true(token_equality(ident, "IDENT", "a"))
    expect_false(token_equality(ident, "IDENT", "b"))
    expect_false(token_equality(ident, "DELIM", "a"))
    expect_true(token_equality(eof, "EOF", NULL))
    # NULL on only one side is FALSE, not logical(0) (an error under
    # && on R >= 4.3) or a zero-length value in a caller's if ()
    expect_false(token_equality(ident, "IDENT", NULL))
    expect_false(token_equality(eof, "EOF", "a"))
})
