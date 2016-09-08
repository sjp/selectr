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
    expect_that(translator$css_to_xpath("xml:lang(en)"), equals("descendant-or-self::xml[lang('en')]"))
    expect_that(translator$css_to_xpath("xml:lang(en-nz)"), equals("descendant-or-self::xml[lang('en-nz')]"))

    expect_error(translator$css_to_xpath("xml:lang()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("xml:lang(1)"), "Expected a single string or ident.*")
    expect_error(translator$css_to_xpath("xml:lang(en, fr)"), "Expected an argument.*")
})

test_that("HTML translator validates language arguments", {
    translator <- HTMLTranslator$new()
    expect_that(translator$css_to_xpath("html:lang(en)"), equals("descendant-or-self::html[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')]]"))
    expect_that(translator$css_to_xpath("html:lang(en-nz)"), equals("descendant-or-self::html[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-nz-')]]"))

    expect_error(translator$css_to_xpath("html:lang()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("html:lang(1)"), "Expected a single string or ident.*")
    expect_error(translator$css_to_xpath("html:lang(en, fr)"), "Expected an argument.*")
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
                equals("descendant-or-self::a[contains(., 'b')]"))
    expect_that(css_to_xpath("a:contains('b')"),
                equals("descendant-or-self::a[contains(., 'b')]"))
    expect_error(css_to_xpath("a:contains(1)"),
                 "Expected a single string or ident for :contains\\(\\), got .*")
})
