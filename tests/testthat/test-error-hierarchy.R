test_that("selectr_translation_error is a structured condition", {
    e <- tryCatch(css_to_xpath("e:frobnicate"), error = identity)
    expect_s3_class(e, "selectr_translation_error")
    expect_s3_class(e, "selectr_error")
    expect_s3_class(e, "error")
    expect_equal(e$selector, "e:frobnicate")
    expect_equal(e$feature, ":frobnicate")
    expect_null(e$call)

    # catchable by a dedicated handler, not just error =
    expect_equal(tryCatch(css_to_xpath("e:frobnicate"),
                          selectr_translation_error = function(e) "structured",
                          error = function(e) "plain"),
                 "structured")

    # message text is unchanged by the new structure
    expect_match(conditionMessage(e), "^The pseudo-class :frobnicate is unknown$")

    # a translator invoked directly (bypassing css_to_xpath()) still
    # gets the selector annotation, since it is added inside
    # GenericTranslator$css_to_xpath() itself
    translator <- GenericTranslator$new()
    e2 <- tryCatch(translator$css_to_xpath("a > :scope"), error = identity)
    expect_s3_class(e2, "selectr_translation_error")
    expect_equal(e2$selector, "a > :scope")
    expect_equal(e2$feature, ":scope")
})

test_that("selectr_argument_error is a structured condition", {
    e <- tryCatch(css_to_xpath(1), error = identity)
    expect_s3_class(e, "selectr_argument_error")
    expect_s3_class(e, "selectr_error")
    expect_s3_class(e, "error")
    expect_null(e$call)
    expect_match(conditionMessage(e), "^The 'selector' argument must be a character vector$")

    # catchable by a dedicated handler, not just error =
    expect_equal(tryCatch(css_to_xpath(1),
                          selectr_argument_error = function(e) "structured",
                          error = function(e) "plain"),
                 "structured")

    # an invalid translator name is wrapped as a structured argument error
    e2 <- tryCatch(css_to_xpath("a", translator = "nope"), error = identity)
    expect_s3_class(e2, "selectr_argument_error")
    expect_match(conditionMessage(e2), "'translator' must be one of")
})

test_that("selectr_parse_error is a selectr_error", {
    # existing coverage lives in test-parse-errors.R; this only checks
    # that it shares the base class with the other two
    e <- tryCatch(css_to_xpath("div >"), error = identity)
    expect_s3_class(e, "selectr_parse_error")
    expect_s3_class(e, "selectr_error")
})
