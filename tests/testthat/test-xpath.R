context("xpath")

test_that("XPathExpr objects print correctly", {
    shw <- function(x) trimws(capture.output(x$show()))

    xp <- XPathExpr$new()
    expect_that(xp$repr(), equals("XPathExpr[*]"))
    expect_that(shw(xp), equals("XPathExpr[*]"))

    xp <- XPathExpr$new("//")
    expect_that(xp$repr(), equals("XPathExpr[//*]"))
    expect_that(shw(xp), equals("XPathExpr[//*]"))

    xp <- XPathExpr$new(element = "a")
    expect_that(xp$repr(), equals("XPathExpr[a]"))
    expect_that(shw(xp), equals("XPathExpr[a]"))

    xp <- XPathExpr$new("//a/", "b")
    expect_that(xp$repr(), equals("XPathExpr[//a/b]"))
    expect_that(shw(xp), equals("XPathExpr[//a/b]"))
})

test_that("Generic translator validates language arguments", {
    translator <- GenericTranslator$new()
    expect_that(translator$css_to_xpath("xml:lang(en)"), equals("descendant-or-self::xml[(lang('en'))]"))
    expect_that(translator$css_to_xpath("xml:lang(en-nz)"), equals("descendant-or-self::xml[(lang('en-nz'))]"))

    expect_error(translator$css_to_xpath("xml:lang()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("xml:lang(1)"), "Expected string, ident, or \\* arguments.*")

    # Multiple languages with OR logic
    expect_that(translator$css_to_xpath("xml:lang(en, fr)"), equals("descendant-or-self::xml[((lang('en') or lang('fr')))]"))
    expect_that(translator$css_to_xpath("xml:lang(en, de, fr)"), equals("descendant-or-self::xml[((lang('en') or lang('de') or lang('fr')))]"))
})

test_that("HTML translator validates language arguments", {
    translator <- HTMLTranslator$new()
    expect_that(translator$css_to_xpath("html:lang(en)"), equals("descendant-or-self::html[(ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')])]"))
    expect_that(translator$css_to_xpath("html:lang(en-nz)"), equals("descendant-or-self::html[(ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-nz-')])]"))

    expect_error(translator$css_to_xpath("html:lang()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("html:lang(1)"), "Expected string, ident, or \\* arguments.*")

    # Multiple languages with OR logic
    expect_that(translator$css_to_xpath("html:lang(en, fr)"), 
                equals("descendant-or-self::html[((ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')] or ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'fr-')]))]"))
})

test_that("Generic translator handles :lang() wildcards and comma lists", {
    translator <- GenericTranslator$new()

    # Simple languages still work
    expect_that(translator$css_to_xpath("div:lang(en)"), equals("descendant-or-self::div[(lang('en'))]"))

    # Wildcard * matches everything
    expect_that(translator$css_to_xpath('div:lang(*)'), equals("descendant-or-self::div[(true())]"))

    # Wildcard suffix like en-* for prefix matching
    expect_that(translator$css_to_xpath('div:lang(en-*)'), equals("descendant-or-self::div[(lang('en-'))]"))
    expect_that(translator$css_to_xpath('div:lang(fr-*)'), equals("descendant-or-self::div[(lang('fr-'))]"))

    # Comma-separated lists with OR logic
    expect_that(translator$css_to_xpath('div:lang(en, fr)'), equals("descendant-or-self::div[((lang('en') or lang('fr')))]"))
    expect_that(translator$css_to_xpath('div:lang(en, de, fr)'), equals("descendant-or-self::div[((lang('en') or lang('de') or lang('fr')))]"))

    # Mixed wildcards and regular languages
    expect_that(translator$css_to_xpath('div:lang(en-*, fr)'), equals("descendant-or-self::div[((lang('en-') or lang('fr')))]"))
    expect_that(translator$css_to_xpath('div:lang(*, de)'), equals("descendant-or-self::div[((true() or lang('de')))]"))
})

test_that("HTML translator handles :lang() wildcards and comma lists", {
    translator <- HTMLTranslator$new()

    # Wildcard * matches any element with lang attribute
    expect_that(translator$css_to_xpath('div:lang(*)'), equals("descendant-or-self::div[(ancestor-or-self::*[@lang])]"))

    # Wildcard suffix for prefix matching
    expect_that(translator$css_to_xpath('div:lang(en-*)'), 
                equals("descendant-or-self::div[(ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')])]"))

    # Multiple values with OR logic
    expect_that(translator$css_to_xpath('div:lang(en, fr)'),
                equals("descendant-or-self::div[((ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')] or ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'fr-')]))]"))
})

test_that("Generic translator handles :dir() function", {
    translator <- GenericTranslator$new()

    # :dir() uses "never matches" pattern (requires runtime directionality detection)
    expect_that(translator$css_to_xpath("div:dir(ltr)"), equals("descendant-or-self::div[(0)]"))
    expect_that(translator$css_to_xpath("div:dir(rtl)"), equals("descendant-or-self::div[(0)]"))
    expect_that(translator$css_to_xpath(":dir(ltr)"), equals("descendant-or-self::*[(0)]"))

    expect_error(translator$css_to_xpath("div:dir()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("div:dir(1)"), "Expected string, ident, or \\* arguments.*")
})

test_that("HTML translator handles :dir() function", {
    translator <- HTMLTranslator$new()

    # :dir() uses "never matches" pattern (requires runtime directionality detection)
    expect_that(translator$css_to_xpath("div:dir(ltr)"), equals("descendant-or-self::div[(0)]"))
    expect_that(translator$css_to_xpath("div:dir(rtl)"), equals("descendant-or-self::div[(0)]"))
    expect_that(translator$css_to_xpath(":dir(ltr)"), equals("descendant-or-self::*[(0)]"))

    expect_error(translator$css_to_xpath("div:dir()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("div:dir(1)"), "Expected string, ident, or \\* arguments.*")
})

test_that("unimplemented methods throw errors", {
    translator <- GenericTranslator$new()

    expect_error(translator$css_to_xpath("*:nth-of-type(2n)"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:nth-last-of-type(2n)"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:first-of-type"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:last-of-type"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:only-of-type"), ".* is not implemented")
})

test_that("contains method only takes string arguments", {
    expect_that(css_to_xpath("a:contains(b)"),
                equals("descendant-or-self::a[(contains(., 'b'))]"))
    expect_that(css_to_xpath("a:contains('b')"),
                equals("descendant-or-self::a[(contains(., 'b'))]"))
    expect_error(css_to_xpath("a:contains(1)"),
                 "Expected a single string or ident for :contains\\(\\), got .*")
})
