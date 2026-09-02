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

test_that("a selector nesting functional pseudo-classes too deeply is caught", {
    # Deeply nested :not() overflows R's expression nesting limit while
    # translating (R >= 4.3 raises expressionStackOverflowError; older R
    # raises a plain error with the same message text); this is caught
    # and re-raised as a structured translation error rather than a raw
    # base-R error. options(expressions=) is lowered so a moderate
    # nesting depth trips the *expression* limit well short of the C
    # stack itself -- how deep that is varies by platform and by how
    # much stack a test runner has already used, so pinning the real
    # default (5000) would make this test's pass/fail depend on where
    # it happens to overflow first
    old_opts <- options(expressions = 450)
    on.exit(options(old_opts))
    deeply_nested <- paste0(strrep(":not(", 300), "a", strrep(")", 300))
    e <- tryCatch(css_to_xpath(deeply_nested), error = identity)
    expect_s3_class(e, "selectr_translation_error")
    expect_equal(conditionMessage(e),
                "selector nests functional pseudo-classes too deeply")
    expect_equal(e$selector, deeply_nested)
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
