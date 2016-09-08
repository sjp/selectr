context("main")

# We know that the results are correct via other tests, just check that
# this produces the correct results with respect to its arguments
test_that("css_to_xpath vectorises arguments", {
    expect_that(css_to_xpath("a b"), equals("descendant-or-self::a/descendant::b"))
    expect_that(css_to_xpath("a b", prefix = ""), equals("a/descendant::b"))
    expect_that(css_to_xpath("a b", prefix = c("descendant-or-self::", "")), equals(c("descendant-or-self::a/descendant::b", "a/descendant::b")))
    expect_that(css_to_xpath("a:checked", prefix = "", translator = c("generic", "html", "xhtml")),
                             equals(c("a[0]", "a[(@selected and name(.) = 'option') or (@checked and (name(.) = 'input' or name(.) = 'command')and (@type = 'checkbox' or @type = 'radio'))]", "a[(@selected and name(.) = 'option') or (@checked and (name(.) = 'input' or name(.) = 'command')and (@type = 'checkbox' or @type = 'radio'))]")))
    expect_that(css_to_xpath(c("a b", "b c"), prefix = ""), equals(c("a/descendant::b", "b/descendant::c")))
})

test_that("css_to_xpath handles bad arguments", {
    # must have a selector arg provided
    expect_error(css_to_xpath(), "A valid selector (character vector) must be provided.", fixed = TRUE)
    expect_error(css_to_xpath(NULL), "A valid selector (character vector) must be provided.", fixed = TRUE)

    # should complain about incorrect vector type
    expect_error(css_to_xpath(1), "The 'selector' argument.*")
    expect_error(css_to_xpath("a", prefix = 1), "The 'prefix' argument.*")
    expect_error(css_to_xpath("a", translator = 1), "The 'translator' argument.*")

    # should strip the NA values out
    expect_that(length(css_to_xpath(c("a", NA))), equals(1))
    expect_that(length(css_to_xpath("a", prefix = c("", NA))), equals(1))
    expect_that(length(css_to_xpath("a", translator = c("generic", NA))), equals(1))

    # expect NAs to be stripped out resulting in zero length args (unusable)
    expect_error(css_to_xpath(NA_character_), "Zero length character vector.*")
    expect_error(css_to_xpath("a", prefix = NA_character_), "Zero length character vector.*")
    expect_error(css_to_xpath("a", translator = NA_character_), "Zero length character vector.*")

    # performs partial matching
    expect_that(css_to_xpath("a", translator = "g"),
                equals("descendant-or-self::a"))
    expect_that(css_to_xpath("a", translator = "gEnErIC"),
                equals("descendant-or-self::a"))
    expect_that(css_to_xpath("a", translator = "h"),
                equals("descendant-or-self::a"))
    expect_that(css_to_xpath("a", translator = "x"),
                equals("descendant-or-self::a"))
    expect_that(css_to_xpath("a", translator = c("g", "h", "x")),
                equals(rep("descendant-or-self::a", 3)))

    # errors anything not matching generic, html, xhtml
    expect_error(css_to_xpath("a", translator = ""), "'arg' should be one of.*")
    expect_error(css_to_xpath("a", translator = "a"), "'arg' should be one of.*")
    expect_error(css_to_xpath("a", translator = c("generic", "a")), "'arg' should be one of.*")
})

test_that("namespace handling works correctly", {
    # formatNS must return a NULL or a named vector
    expect_that(formatNS(NULL), equals(NULL))
    expect_that(formatNS(list(a = "b")), equals(c(a = "b")))
    expect_that(formatNS(c(a = "b")), equals(c(a = "b")))

    # bad input handling
    expect_error(formatNS(1), "A namespace object must be.*")
    expect_error(formatNS(TRUE), "A namespace object must be.*")

    expect_error(formatNS("a"), "The namespace object either missing some or all names.*")
    expect_error(formatNS(c(a = "a", "b")), "The namespace object either missing some or all names.*")
    tmp <- letters
    names(tmp) <- letters[1:5]
    expect_error(formatNS(tmp), "The namespace object either missing some or all names.*")
    expect_error(formatNS(list(a = 1, b = 2)), "The values in the namespace object.*")

    # formatNSPrefix must return a pipe separated string of namespace prefixes
    expect_that(formatNSPrefix(c(svg = "svg"), ""), equals("(//svg:*)/"))
    expect_that(formatNSPrefix(c(svg = "svg"), "asd"), equals("(//svg:*)/asd"))
    expect_that(formatNSPrefix(c(svg = "svg", math = "mathml"), ""), equals("(//svg:*|//math:*)/"))
    expect_that(formatNSPrefix(c(svg = "svg", math = "mathml"), "asd"), equals("(//svg:*|//math:*)/asd"))
})
