context("main")

# We know that the results are correct via other tests, just check that
# this produces the correct results with respect to its arguments
test_that("css_to_xpath vectorises arguments", {
    expect_that(css_to_xpath("a b"), equals("descendant-or-self::a/descendant-or-self::*/b"))
    expect_that(css_to_xpath("a b", prefix = ""), equals("a/descendant-or-self::*/b"))
    expect_that(css_to_xpath("a b"), equals("descendant-or-self::a/descendant-or-self::*/b", "a/descendant-or-self::*/b"))
    expect_that(css_to_xpath("a:checked", prefix = "", translator = c("generic", "html")),
                             equals(c("a[0]", "a[(@selected and name(.) = 'option') or (@checked and (name(.) = 'input' or name(.) = 'command')and (@type = 'checkbox' or @type = 'radio'))]")))
    expect_that(css_to_xpath(c("a b", "b c"), prefix = ""), equals(c("a/descendant-or-self::*/b", "b/descendant-or-self::*/c")))
})
