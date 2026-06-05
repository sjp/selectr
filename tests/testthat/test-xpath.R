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
    expect_error(translator$css_to_xpath("xml:lang(1)"), "Expected string, ident, or \\* arguments.*")
    # The reported argument is the offending one, not the first
    expect_error(translator$css_to_xpath("xml:lang(en, 5)"),
                 "Expected string, ident, or \\* arguments for :lang\\(\\), got <NUMBER '5' at 14>")

    # Multiple languages with OR logic
    expect_that(translator$css_to_xpath("xml:lang(en, fr)"), equals("descendant-or-self::xml[lang('en') or lang('fr')]"))
    expect_that(translator$css_to_xpath("xml:lang(en, de, fr)"), equals("descendant-or-self::xml[lang('en') or lang('de') or lang('fr')]"))
})

test_that("HTML translator validates language arguments", {
    translator <- HTMLTranslator$new()
    expect_that(translator$css_to_xpath("html:lang(en)"), equals("descendant-or-self::html[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')]]"))
    expect_that(translator$css_to_xpath("html:lang(en-nz)"), equals("descendant-or-self::html[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-nz-')]]"))

    expect_error(translator$css_to_xpath("html:lang()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("html:lang(1)"), "Expected string, ident, or \\* arguments.*")

    # Multiple languages with OR logic
    expect_that(translator$css_to_xpath("html:lang(en, fr)"),
                equals("descendant-or-self::html[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')] or ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'fr-')]]"))
})

test_that("HTML translator lowercases attribute names but not values", {
    translator <- HTMLTranslator$new()

    # Attribute names in HTML are case-insensitive, but values are not
    expect_that(translator$css_to_xpath('[Data-State="Active"]'),
                equals("descendant-or-self::*[@data-state = 'Active']"))
    expect_that(translator$css_to_xpath('[data-state~="Active"]'),
                equals(paste0("descendant-or-self::*[@data-state and ",
                              "contains(concat(' ', ",
                              "normalize-space(@data-state), ' '), ",
                              "' Active ')]")))
    # Element names are still lowercased
    expect_that(translator$css_to_xpath('DIV[data-state="Active"]'),
                equals("descendant-or-self::div[@data-state = 'Active']"))
})

test_that("Generic translator handles :lang() wildcards and comma lists", {
    translator <- GenericTranslator$new()

    # Simple languages still work
    expect_that(translator$css_to_xpath("div:lang(en)"), equals("descendant-or-self::div[lang('en')]"))

    # Wildcard * matches everything
    expect_that(translator$css_to_xpath('div:lang(*)'), equals("descendant-or-self::div[true()]"))

    # Wildcard suffix like en-* for prefix matching; the trailing "-*" is
    # stripped because XPath's lang() already matches at '-' boundaries
    # (lang('en-') would match nothing)
    expect_that(translator$css_to_xpath('div:lang(en-*)'), equals("descendant-or-self::div[lang('en')]"))
    expect_that(translator$css_to_xpath('div:lang(fr-*)'), equals("descendant-or-self::div[lang('fr')]"))

    # Comma-separated lists with OR logic
    expect_that(translator$css_to_xpath('div:lang(en, fr)'), equals("descendant-or-self::div[lang('en') or lang('fr')]"))
    expect_that(translator$css_to_xpath('div:lang(en, de, fr)'), equals("descendant-or-self::div[lang('en') or lang('de') or lang('fr')]"))

    # Mixed wildcards and regular languages
    expect_that(translator$css_to_xpath('div:lang(en-*, fr)'), equals("descendant-or-self::div[lang('en') or lang('fr')]"))
    expect_that(translator$css_to_xpath('div:lang(*, de)'), equals("descendant-or-self::div[true() or lang('de')]"))
})

test_that("HTML translator handles :lang() wildcards and comma lists", {
    translator <- HTMLTranslator$new()

    # Wildcard * matches any element with lang attribute
    expect_that(translator$css_to_xpath('div:lang(*)'), equals("descendant-or-self::div[ancestor-or-self::*[@lang]]"))

    # Wildcard suffix for prefix matching
    expect_that(translator$css_to_xpath('div:lang(en-*)'),
                equals("descendant-or-self::div[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')]]"))

    # Multiple values with OR logic
    expect_that(translator$css_to_xpath('div:lang(en, fr)'),
                equals("descendant-or-self::div[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')] or ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'fr-')]]"))
})

test_that("Generic translator handles :dir() function", {
    translator <- GenericTranslator$new()

    # :dir() uses "never matches" pattern (requires runtime directionality detection)
    expect_that(translator$css_to_xpath("div:dir(ltr)"), equals("descendant-or-self::div[0]"))
    expect_that(translator$css_to_xpath("div:dir(rtl)"), equals("descendant-or-self::div[0]"))
    expect_that(translator$css_to_xpath(":dir(ltr)"), equals("descendant-or-self::*[0]"))
    # values other than ltr/rtl are not invalid, they just never match
    expect_that(translator$css_to_xpath(":dir(foo)"), equals("descendant-or-self::*[0]"))

    expect_error(translator$css_to_xpath("div:dir()"), "Expected at least one argument.*")
    # :dir() takes exactly one identifier (CSS Selectors Level 4)
    expect_error(translator$css_to_xpath("div:dir(1)"), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath('div:dir("ltr")'), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath("div:dir(ltr rtl)"), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath("div:dir(ltr, rtl)"), "Expected an argument.*")
    expect_error(translator$css_to_xpath("div:dir(*)"), "Expected an argument.*")
})

test_that("HTML translator handles :dir() function", {
    translator <- HTMLTranslator$new()

    # :dir() never matches with the HTML translators too - a
    # deliberate decision, not a missing override: resolved
    # directionality (dir=auto, bdi, form controls) is not static,
    # so no :lang()-style attribute-walk approximation is attempted
    expect_that(translator$css_to_xpath("div:dir(ltr)"), equals("descendant-or-self::div[0]"))
    expect_that(translator$css_to_xpath("div:dir(rtl)"), equals("descendant-or-self::div[0]"))
    expect_that(translator$css_to_xpath(":dir(ltr)"), equals("descendant-or-self::*[0]"))

    xhtml_translator <- HTMLTranslator$new(xhtml = TRUE)
    expect_that(xhtml_translator$css_to_xpath("div:dir(ltr)"),
                equals("descendant-or-self::div[0]"))
    expect_that(xhtml_translator$css_to_xpath("div:dir(rtl)"),
                equals("descendant-or-self::div[0]"))

    expect_error(translator$css_to_xpath("div:dir()"), "Expected at least one argument.*")
    # :dir() takes exactly one identifier (CSS Selectors Level 4)
    expect_error(translator$css_to_xpath("div:dir(1)"), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath('div:dir("ltr")'), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath("div:dir(ltr rtl)"), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath("div:dir(ltr, rtl)"), "Expected an argument.*")
    expect_error(translator$css_to_xpath("div:dir(*)"), "Expected an argument.*")
})

test_that(":lang() and :dir() reject a lone '-' argument", {
    # A lone '-' is not a valid <ident> per css-syntax (an ident may
    # start with '-' only when followed by an ident-start code point
    # or a second '-')
    for (translator in list(GenericTranslator$new(), HTMLTranslator$new())) {
        expect_error(translator$css_to_xpath("e:lang(-)"),
                     "Expected string, ident, or \\* arguments.*")
        expect_error(translator$css_to_xpath("e:dir(-)"),
                     "Expected a single ident argument.*")
        expect_error(translator$css_to_xpath("e:lang(en, -)"),
                     "Expected string, ident, or \\* arguments.*")
        # valid idents starting or ending with '-' keep working
        expect_error(translator$css_to_xpath("e:lang(--x)"), NA)
        expect_error(translator$css_to_xpath("e:lang(en--)"), NA)
        expect_error(translator$css_to_xpath("e:lang(en-*)"), NA)
    }
})

test_that("HTMLTranslator rejects unknown construction arguments", {
    expect_error(HTMLTranslator$new(strict = TRUE), "unused argument")
    # (xhtm = TRUE would still construct via R's standard partial
    # argument matching of xhtml)
    expect_that(HTMLTranslator$new(xhtm = TRUE)$xhtml, equals(TRUE))
})

test_that("a translator subclass can add new pseudo-class handlers", {
    # Dispatch is dynamic, so a handler defined only on a subclass is
    # found without editing the base class
    BlinkTranslator <- R6::R6Class("BlinkTranslator",
        inherit = GenericTranslator,
        public = list(
            xpath_blink_pseudo = function(xpath) {
                xpath$add_condition("@blink")
                xpath
            },
            xpath_nth_word_function = function(xpath, fn) {
                xpath$add_condition("@nth-word")
                xpath
            }))

    translator <- BlinkTranslator$new()
    expect_that(translator$css_to_xpath("a:blink"),
                equals("descendant-or-self::a[@blink]"))
    expect_that(translator$css_to_xpath("a:nth-word(2)"),
                equals("descendant-or-self::a[@nth-word]"))
    # Unknown names still produce the usual errors
    expect_error(translator$css_to_xpath("a:frobnicate"),
                 "The pseudo-class :frobnicate is unknown")
    expect_error(translator$css_to_xpath("a:frobnicate(2)"),
                 "The pseudo-class :frobnicate\\(\\) is unknown")
})

test_that("a translator subclass can override id_attribute", {
    XMLIdTranslator <- R6::R6Class("XMLIdTranslator",
        inherit = GenericTranslator,
        public = list(id_attribute = "xml:id"))

    expect_that(XMLIdTranslator$new()$css_to_xpath("#foo"),
                equals("descendant-or-self::*[@xml:id = 'foo']"))
    # The default is unchanged
    expect_that(GenericTranslator$new()$css_to_xpath("#foo"),
                equals("descendant-or-self::*[@id = 'foo']"))
})

test_that("unimplemented methods throw errors", {
    translator <- GenericTranslator$new()

    expect_error(translator$css_to_xpath("*:nth-of-type(2n)"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:nth-last-of-type(2n)"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:first-of-type"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:last-of-type"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:only-of-type"), ".* is not implemented")
})