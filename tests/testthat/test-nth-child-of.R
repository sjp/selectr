context("nth-child with 'of S' selector list (CSS Level 4)")

test_that(":nth-child(n of S) parses correctly", {
    parsed <- selectr:::parse("div:nth-child(2 of .foo)")
    expect_equal(length(parsed), 1)

    fn_obj <- parsed[[1]]$parsed_tree
    expect_equal(class(fn_obj)[1], "Function")
    expect_equal(fn_obj$name, "nth-child")
    expect_false(is.null(fn_obj$selector_list))
    expect_equal(length(fn_obj$selector_list), 1)
})

test_that(":nth-last-child(n of S) parses correctly", {
    parsed <- selectr:::parse("li:nth-last-child(3 of .important)")
    expect_equal(length(parsed), 1)

    fn_obj <- parsed[[1]]$parsed_tree
    expect_equal(class(fn_obj)[1], "Function")
    expect_equal(fn_obj$name, "nth-last-child")
    expect_false(is.null(fn_obj$selector_list))
    expect_equal(length(fn_obj$selector_list), 1)
})

test_that(":nth-child(n of S) with multiple selectors parses correctly", {
    parsed <- selectr:::parse("div:nth-child(2 of .foo, .bar)")
    expect_equal(length(parsed), 1)

    fn_obj <- parsed[[1]]$parsed_tree
    expect_equal(fn_obj$name, "nth-child")
    expect_equal(length(fn_obj$selector_list), 2)
})

test_that(":nth-child(n of S) generates correct XPath", {
    xpath <- css_to_xpath("div:nth-child(2 of .foo)")

    # Should count siblings matching .foo
    expect_true(grepl("count\\(preceding-sibling::\\*\\[", xpath))
    expect_true(grepl("@class", xpath))
    expect_true(grepl("foo", xpath))

    # Should also check that current element matches .foo
    expect_true(grepl("and.*@class", xpath))
})

test_that(":nth-last-child(n of S) generates correct XPath", {
    xpath <- css_to_xpath("li:nth-last-child(3 of .important)")

    # Should count following siblings matching .important
    expect_true(grepl("count\\(following-sibling::\\*\\[", xpath))
    expect_true(grepl("@class", xpath))
    expect_true(grepl("important", xpath))

    # Should also check that current element matches .important
    expect_true(grepl("and.*@class", xpath))
})

test_that(":nth-child(An+B of S) with formula works", {
    xpath <- css_to_xpath("p:nth-child(2n+1 of .highlight)")

    # Should have modulo operation for 2n+1
    expect_true(grepl("mod", xpath))

    # Should filter by .highlight
    expect_true(grepl("highlight", xpath))
})

test_that(":nth-child(n of S1, S2) with multiple selectors generates OR condition", {
    xpath <- css_to_xpath("div:nth-child(1 of .foo, .bar)")

    # Should have both selectors
    expect_true(grepl("foo", xpath))
    expect_true(grepl("bar", xpath))

    # Should have OR condition
    expect_true(grepl("or", xpath))
})

test_that("Regular :nth-child without 'of' still works", {
    xpath1 <- css_to_xpath("div:nth-child(2)")
    xpath2 <- css_to_xpath("div:nth-last-child(3)")

    # Should not have class checks
    expect_false(grepl("@class", xpath1))
    expect_false(grepl("@class", xpath2))

    # Should have simple counting
    expect_true(grepl("count\\(preceding-sibling::\\*\\)", xpath1))
    expect_true(grepl("count\\(following-sibling::\\*\\)", xpath2))
})

test_that(":nth-child(odd of S) works", {
    xpath <- css_to_xpath("div:nth-child(odd of .item)")

    # Should have modulo 2
    expect_true(grepl("mod 2", xpath))

    # Should filter by .item
    expect_true(grepl("item", xpath))
})

test_that(":nth-child(even of S) works", {
    xpath <- css_to_xpath("div:nth-child(even of .item)")

    # Should have modulo 2
    expect_true(grepl("mod 2", xpath))

    # Should filter by .item
    expect_true(grepl("item", xpath))
})

test_that(":nth-child with complex selector works", {
    xpath <- css_to_xpath("div:nth-child(2 of div.foo)")

    # Should check element name
    expect_true(grepl("name\\(\\) = 'div'", xpath))

    # Should check class
    expect_true(grepl("foo", xpath))
})
