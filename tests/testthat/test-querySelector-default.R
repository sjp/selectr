context("querySelector-default")

test_that("querySelector methods present an error on non-XML/xml2 objects", {
    expect_error(querySelector(list()), "The object given to querySelector.*")
    expect_error(querySelectorAll(list()), "The object given to querySelector.*")
    expect_error(querySelectorNS(list()), "The object given to querySelector.*")
    expect_error(querySelectorAllNS(list()), "The object given to querySelector.*")
})
