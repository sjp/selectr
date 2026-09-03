# We know that the results are correct via other tests, just check that
# this produces the correct results with respect to its arguments
test_that("css_to_xpath vectorises arguments", {
    expect_equal(css_to_xpath("a b"), "descendant-or-self::a//b")
    expect_equal(css_to_xpath("a b", prefix = ""), "a//b")
    expect_equal(css_to_xpath("a b", prefix = c("descendant-or-self::", "")), c("descendant-or-self::a//b", "a//b"))
    fold <- "translate(@type, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"
    # 'a' is not in ':checked''s element set (option, input), so the HTML
    # translators prune the whole predicate down to a bare '0' - see
    # add_disjunction() in R/xpath.R
    expect_equal(css_to_xpath("a:checked", prefix = "", translator = c("generic", "html", "xhtml")),
                              c("a[0]", "a[0]", "a[0]"))
    checked_html <- paste0(
        "input[@checked and (", fold, " = 'checkbox' or ", fold, " = 'radio')]")
    expect_equal(css_to_xpath("input:checked", prefix = "", translator = c("generic", "html", "xhtml")),
                              c("input[0]", checked_html, checked_html))
    expect_equal(css_to_xpath(c("a b", "b c"), prefix = ""), c("a//b", "b//c"))
})

test_that("css_to_xpath translates duplicate selectors only once per call", {
    ns <- environment(css_to_xpath)
    parses <- 0L
    suppressMessages(trace("parse", where = ns, print = FALSE,
                           tracer = function() parses <<- parses + 1L))
    on.exit(suppressMessages(untrace("parse", where = ns)))

    expect_equal(css_to_xpath(c("#a", "#b", "#a"), prefix = ""),
                 c("*[@id = 'a']", "*[@id = 'b']", "*[@id = 'a']"))
    expect_equal(parses, 2L)

    # A repeated selector still re-parses when the prefix or
    # translator differs, and the de-duplication does not persist
    # across calls
    parses <- 0L
    expect_equal(css_to_xpath(c("#a", "#a"), prefix = c("", "p//")),
                 c("*[@id = 'a']", "p//*[@id = 'a']"))
    expect_equal(css_to_xpath("#a", prefix = ""), "*[@id = 'a']")
    expect_equal(parses, 3L)

    # The length-prefixed key cannot confuse selector/prefix boundaries
    expect_equal(xpath_cache_key("a", "b//", "generic") ==
                 xpath_cache_key("a\r1\rb", "//", "generic"),
                 FALSE)

    # A selector whose key exceeds the 10000 byte limit on R symbols
    # is translated uncached rather than failing the lookup
    long <- paste(rep("a", 4000), collapse = " > ")
    expected <- paste0("descendant-or-self::",
                       paste(rep("a", 4000), collapse = "/"))
    expect_true(nchar(xpath_cache_key(long, "", "generic")) > 10000)
    expect_equal(css_to_xpath(c(long, long)), c(expected, expected))
})

test_that("css_to_xpath handles bad arguments", {
    # must have a selector arg provided
    expect_error(css_to_xpath(), "A valid selector (character vector) must be provided.", fixed = TRUE)
    expect_error(css_to_xpath(NULL), "A valid selector (character vector) must be provided.", fixed = TRUE)

    # should complain about incorrect vector type
    expect_error(css_to_xpath(1), "The 'selector' argument.*")
    expect_error(css_to_xpath("a", prefix = 1), "The 'prefix' argument.*")
    expect_error(css_to_xpath("a", translator = 1), "The 'translator' argument.*")

    # NA values are not allowed in any argument
    expect_error(css_to_xpath(c("a", NA)), "NA values are not allowed in the 'selector' argument")
    expect_error(css_to_xpath("a", prefix = c("", NA)), "NA values are not allowed in the 'prefix' argument")
    expect_error(css_to_xpath("a", translator = c("generic", NA)), "NA values are not allowed in the 'translator' argument")
    expect_error(css_to_xpath(NA_character_), "NA values are not allowed in the 'selector' argument")
    expect_error(css_to_xpath("a", prefix = NA_character_), "NA values are not allowed in the 'prefix' argument")
    expect_error(css_to_xpath("a", translator = NA_character_), "NA values are not allowed in the 'translator' argument")

    # zero length arguments are unusable
    expect_error(css_to_xpath(character(0)), "Zero length character vector.*")
    expect_error(css_to_xpath("a", prefix = character(0)), "Zero length character vector.*")
    expect_error(css_to_xpath("a", translator = character(0)), "Zero length character vector.*")

    # arguments are only recycled from length 1, so a partially
    # recycled (or over-long) argument is an error rather than a
    # plausible-looking result
    expect_error(css_to_xpath(c("a", "b"), prefix = c("", "", "//")),
                 "Arguments must have length 1 or a common length \\(3\\).*selector \\(length 2\\)")
    expect_error(css_to_xpath(c("a", "b", "c", "d"), prefix = c("", "//")),
                 "Arguments must have length 1 or a common length \\(4\\).*prefix \\(length 2\\)")
    expect_error(css_to_xpath(c("a", "b"), prefix = c("", "//", ""),
                              translator = c("generic", "html")),
                 "arguments do not: selector \\(length 2\\), translator \\(length 2\\)")

    # length 1 arguments are still broadcast to the common length
    expect_equal(css_to_xpath(c("a", "b"), prefix = ""),
                 c("a", "b"))
    expect_equal(css_to_xpath("a", prefix = c("", "//")),
                 c("a", "//a"))
    expect_equal(css_to_xpath(c("a", "b"), prefix = c("", "//"),
                              translator = "html"),
                 c("a", "//b"))

    # performs partial matching
    expect_equal(css_to_xpath("a", translator = "g"),
                 "descendant-or-self::a")
    expect_equal(css_to_xpath("a", translator = "gEnErIC"),
                 "descendant-or-self::a")
    expect_equal(css_to_xpath("a", translator = "h"),
                 "descendant-or-self::a")
    expect_equal(css_to_xpath("a", translator = "x"),
                 "descendant-or-self::a")
    expect_equal(css_to_xpath("a", translator = c("g", "h", "x")),
                 rep("descendant-or-self::a", 3))

    # errors anything not matching generic, html, xhtml
    expect_error(css_to_xpath("a", translator = ""), "'translator' must be one of.*")
    expect_error(css_to_xpath("a", translator = "a"), "'translator' must be one of.*")
    expect_error(css_to_xpath("a", translator = c("generic", "a")), "'translator' must be one of.*")
})

test_that("css_to_xpath rejects invalid bytes as a selectr_error", {
    skip_if_not(l10n_info()[["UTF-8"]])

    bad <- "a\xff"
    err <- tryCatch(css_to_xpath(bad), error = function(e) e)
    expect_s3_class(err, "selectr_argument_error")
    expect_match(conditionMessage(err), "invalid or non-convertible bytes")

    err <- tryCatch(css_to_xpath("a", prefix = bad), error = function(e) e)
    expect_s3_class(err, "selectr_argument_error")
    expect_match(conditionMessage(err), "invalid or non-convertible bytes")

    # a validly-marked non-UTF-8 string is still accepted and
    # transcoded to UTF-8 output
    x <- iconv("é", "UTF-8", "latin1")
    expect_equal(Encoding(x), "latin1")
    result <- css_to_xpath(x)
    expect_equal(Encoding(result), "UTF-8")
})

test_that("namespace handling works correctly", {
    # formatNS must return a NULL or a named vector
    expect_equal(formatNS(NULL), NULL)
    expect_equal(formatNS(list(a = "b")), c(a = "b"))
    expect_equal(formatNS(c(a = "b")), c(a = "b"))
    # a zero-length namespace object means "no namespaces", so it is
    # not validated for names and passes straight through
    expect_equal(formatNS(character(0)), character(0))
    expect_equal(formatNS(list()), character(0))

    # bad input handling
    expect_error(formatNS(1), "A namespace object must be.*")
    expect_error(formatNS(TRUE), "A namespace object must be.*")

    expect_error(formatNS("a"), "The namespace object must be a named list or character vector.*")
    expect_error(formatNS(c(a = "a", "b")), "The namespace object must be a named list or character vector.*")
    tmp <- letters
    names(tmp) <- letters[1:5]
    expect_error(formatNS(tmp), "The namespace object must be a named list or character vector.*")
    expect_error(formatNS(list(a = 1, b = 2)), "The values in the namespace object.*")
    # multi-element values would misalign every subsequent prefix after
    # unlist(), e.g. "u2" silently becoming namespace "b"
    expect_error(formatNS(list(a = c("u1", "u2"), b = "u3")),
                 "Each element in the namespace object must be a single character string.")
    expect_error(formatNS(list(a = character(0), b = "u3")),
                 "Each element in the namespace object must be a single character string.")
    # values must be non-missing, non-empty strings, otherwise NA/"" get
    # passed straight through to xml2::xml_find_all()/XML::getNodeSet() as a URI
    expect_error(formatNS(list(a = NA_character_)),
                 "The values in the namespace object must be non-missing, non-empty strings.")
    expect_error(formatNS(c(a = "")),
                 "The values in the namespace object must be non-missing, non-empty strings.")

    # formatNSPrefix must return a pipe separated string of namespace
    # prefixes, relative to the node the query starts from
    expect_equal(formatNSPrefix(c(svg = "svg"), ""),
                 "(descendant-or-self::svg:*)/")
    expect_equal(formatNSPrefix(c(svg = "svg"), "asd"),
                 "(descendant-or-self::svg:*)/asd")
    expect_equal(formatNSPrefix(c(svg = "svg", math = "mathml"), ""),
                 "(descendant-or-self::svg:*|descendant-or-self::math:*)/")
    expect_equal(formatNSPrefix(c(svg = "svg", math = "mathml"), "asd"),
                 "(descendant-or-self::svg:*|descendant-or-self::math:*)/asd")
})
