test_that("querySelector methods present an error on non-XML/xml2 objects", {
    expect_error(querySelector(list()), class = "selectr_argument_error")
    expect_error(querySelectorAll(list()), class = "selectr_argument_error")
    expect_error(querySelectorNS(list()), class = "selectr_argument_error")
    expect_error(querySelectorAllNS(list()), class = "selectr_argument_error")
})
