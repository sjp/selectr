context("parser")

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

    expect_that(parse_many("*"), equals("Element[*]"))
    expect_that(parse_many("*|*"), equals("Element[*]"))
    expect_that(parse_many("*|foo"), equals("Element[foo]"))
    expect_that(parse_many("|foo"), equals("Element[foo]"))
    expect_that(parse_many("foo|*"), equals("Element[foo|*]"))
    expect_that(parse_many("foo|bar"), equals("Element[foo|bar]"))
    expect_that(parse_many('foo[lang|="zh"]'), equals("Attrib[Element[foo][lang |= 'zh']]"))
    # This will never match, but it is valid:
    expect_that(parse_many("#foo#bar"),
                equals("Hash[Hash[Element[*]#foo]#bar]"))
    expect_that(parse_many(c("div>.foo",
                             "div> .foo",
                             "div >.foo",
                             "div > .foo",
                             "div > .foo",
                             "div \n>  \t \t .foo",
                             "div\r>\n\n\n.foo",
                             "div\f>\f.foo")),
                equals(rep("CombinedSelector[Element[div] > Class[Element[*].foo]]", 8)))
    expect_that(parse_many(c("td.foo,.bar",
                             "td.foo, .bar",
                             "td.foo\t\r\n\f ,\t\r\n\f .bar")),
                equals(rep(c("Class[Element[td].foo]",
                             "Class[Element[*].bar]"), 3)))
    expect_that(parse_many(c("div, td.foo, div.bar span")),
                equals(c("Element[div]",
                         "Class[Element[td].foo]",
                         "CombinedSelector[Class[Element[div].bar] <followed> Element[span]]")))
    expect_that(parse_many("div > p"),
                equals("CombinedSelector[Element[div] > Element[p]]"))
    expect_that(parse_many("td:first"),
                equals("Pseudo[Element[td]:first]"))
    expect_that(parse_many("td :first"),
                equals("CombinedSelector[Element[td] <followed> Pseudo[Element[*]:first]]"))
    expect_that(parse_many(c("a[name]", "a[ name\t]")),
                equals(rep("Attrib[Element[a][name]]", 2)))
    expect_that(parse_many("a [name]"),
                equals("CombinedSelector[Element[a] <followed> Attrib[Element[*][name]]]"))
    expect_that(parse_many(c('a[rel="include"]', 'a[rel = include]')),
                equals(rep("Attrib[Element[a][rel = 'include']]", 2)))
    expect_that(parse_many(c("a[hreflang |= 'en']", "a[hreflang|=en]")),
                equals(rep("Attrib[Element[a][hreflang |= 'en']]", 2)))
    expect_that(parse_many("div:nth-child(10)"),
                equals("Function[Element[div]:nth-child(['10'])]"))
    expect_that(parse_many(":nth-child(2n+2)"),
                equals("Function[Element[*]:nth-child(['2', 'n', '+2'])]"))
    expect_that(parse_many("div:nth-of-type(10)"),
                equals("Function[Element[div]:nth-of-type(['10'])]"))
    expect_that(parse_many("div div:nth-of-type(10) .aclass"),
                equals("CombinedSelector[CombinedSelector[Element[div] <followed> Function[Element[div]:nth-of-type(['10'])]] <followed> Class[Element[*].aclass]]"))
    expect_that(parse_many("label:only"),
                equals("Pseudo[Element[label]:only]"))
    expect_that(parse_many("a:lang(fr)"),
                equals("Function[Element[a]:lang(['fr'])]"))
    expect_that(parse_many('div:contains("foo")'),
                equals("Function[Element[div]:contains(['foo'])]"))
    expect_that(parse_many("div#foobar"),
                equals("Hash[Element[div]#foobar]"))
    expect_that(parse_many("div:not(div.foo)"),
                equals("Negation[Element[div]:not(Class[Element[div].foo])]"))
    expect_that(parse_many("td ~ th"),
                equals("CombinedSelector[Element[td] ~ Element[th]]"))

    # handle comments
    expect_that(parse_many("a /* test */"),
                equals("Element[a]"))
    expect_that(parse_many("a/* test */"),
                equals("Element[a]"))
    expect_that(parse_many("/* test */ a"),
                equals("Element[a]"))
    expect_that(parse_many("/* test */a"),
                equals("Element[a]"))
    expect_that(parse_many("a /* test */ b"),
                equals("CombinedSelector[Element[a] <followed> Element[b]]"))
    expect_that(parse_many("a /* test "),
                equals("Element[a]"))
})

test_that("parsed elements print correctly", {
    shw <- function(x) trimws(capture.output(parse(x)[[1]]$show()))

    expect_that(shw("a"), equals("Element[a]"))
    expect_that(shw(".test"), equals("Class[Element[*].test]"))
    expect_that(shw(":active"), equals("Pseudo[Element[*]:active]"))
    expect_that(shw("a:not(.toggle)"), equals("Negation[Element[a]:not(Class[Element[*].toggle])]"))
    expect_that(shw("[href]"), equals("Attrib[Element[*][href]]"))
    expect_that(shw("#id"), equals("Hash[Element[*]#id]"))
})

test_that("compiled regex parsing functions behave as expected", {
    m_whitespace <- compile_('[ \t\r\n\f]+')
    m_number <- compile_('[+-]?(?:[0-9]*\\.[0-9]+|[0-9]+)')
    m_hash <- compile_(paste0("^#([_a-zA-Z0-9-]|", nonascii, "|\\\\(?:", delim_escapes, "))+"))
    m_ident <- compile_(paste0("^([_a-zA-Z0-9-]|", nonascii, "|\\\\(?:", delim_escapes, "))+"))

    expect_that(m_whitespace("a b"), equals(match_whitespace("a b")))
    expect_that(m_number("a 1"), equals(match_number("a 1")))
    expect_that(m_hash("a #test"), equals(match_hash("a #test")))
    expect_that(m_ident(" test"), equals(match_ident(" test")))
})
