test_that("every xpath_*_pseudo/_function/_combinator method is on the man/selectors.Rd reference", {
    # Reflects over the translators' public methods so the reference page
    # cannot silently drift when a pseudo-class, function-notation
    # pseudo-class or combinator is added, renamed or removed - see the
    # method naming convention in R/xpath.R.
    all_methods <- union(names(GenericTranslator$public_methods),
                          names(HTMLTranslator$public_methods))
    reflected <- grep("^xpath_.*_(pseudo|function|combinator)$", all_methods,
                       value = TRUE)

    # Kept in sync by hand with man/selectors.Rd; a mismatch here means
    # that page needs a row added (or this list trimmed) for the method.
    documented <- c(
        "xpath_active_pseudo", "xpath_any_link_pseudo", "xpath_checked_pseudo",
        "xpath_child_combinator", "xpath_default_pseudo",
        "xpath_descendant_combinator", "xpath_dir_function",
        "xpath_direct_adjacent_combinator", "xpath_disabled_pseudo",
        "xpath_empty_pseudo", "xpath_enabled_pseudo", "xpath_first_child_pseudo",
        "xpath_first_of_type_pseudo", "xpath_focus_pseudo",
        "xpath_focus_visible_pseudo", "xpath_focus_within_pseudo",
        "xpath_hover_pseudo", "xpath_indirect_adjacent_combinator",
        "xpath_lang_function", "xpath_last_child_pseudo",
        "xpath_last_of_type_pseudo", "xpath_link_pseudo",
        "xpath_local_link_pseudo", "xpath_nth_child_function",
        "xpath_nth_last_child_function", "xpath_nth_last_of_type_function",
        "xpath_nth_of_type_function", "xpath_only_child_pseudo",
        "xpath_only_of_type_pseudo", "xpath_optional_pseudo",
        "xpath_placeholder_shown_pseudo", "xpath_read_only_pseudo",
        "xpath_read_write_pseudo", "xpath_required_pseudo", "xpath_root_pseudo",
        "xpath_scope_pseudo", "xpath_target_pseudo",
        "xpath_target_within_pseudo", "xpath_visited_pseudo"
    )

    expect_setequal(reflected, documented)
})

test_that("man/selectors.Rd combinator examples match live translation", {
    gt <- GenericTranslator$new()
    expect_equal(gt$css_to_xpath("e f"), "descendant-or-self::e//f")
    expect_equal(gt$css_to_xpath("e > f"), "descendant-or-self::e/f")
    expect_equal(gt$css_to_xpath("e + f"),
                 "descendant-or-self::e/following-sibling::*[1][self::f]")
    expect_equal(gt$css_to_xpath("e ~ f"),
                 "descendant-or-self::e/following-sibling::f")
    expect_error(gt$css_to_xpath("e || f"))
})

test_that("man/selectors.Rd simple-selector examples match live translation", {
    gt <- GenericTranslator$new()
    expect_equal(gt$css_to_xpath("*"), "descendant-or-self::*")
    expect_equal(gt$css_to_xpath("e"), "descendant-or-self::e")
    expect_equal(gt$css_to_xpath(".class"),
                 "descendant-or-self::*[contains(concat(' ', normalize-space(@class), ' '), ' class ')]")
    expect_equal(gt$css_to_xpath("#id"), "descendant-or-self::*[@id = 'id']")
})

test_that("man/selectors.Rd attribute-selector examples match live translation", {
    gt <- GenericTranslator$new()
    expect_equal(gt$css_to_xpath("[attr]"), "descendant-or-self::*[@attr]")
    expect_equal(gt$css_to_xpath("[attr=val]"),
                 "descendant-or-self::*[@attr = 'val']")
    expect_equal(gt$css_to_xpath("[attr~=val]"),
                 "descendant-or-self::*[contains(concat(' ', normalize-space(@attr), ' '), ' val ')]")
    expect_equal(gt$css_to_xpath("[attr|=val]"),
                 "descendant-or-self::*[@attr = 'val' or starts-with(@attr, 'val-')]")
    expect_equal(gt$css_to_xpath("[attr^=val]"),
                 "descendant-or-self::*[starts-with(@attr, 'val')]")
    expect_equal(gt$css_to_xpath("[attr$=val]"),
                 "descendant-or-self::*[substring(@attr, string-length(@attr)-2) = 'val']")
    expect_equal(gt$css_to_xpath("[attr*=val]"),
                 "descendant-or-self::*[contains(@attr, 'val')]")
    expect_equal(gt$css_to_xpath("[attr=val i]"),
                 "descendant-or-self::*[translate(@attr, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz') = 'val']")
    expect_equal(gt$css_to_xpath("[attr=val s]"), "descendant-or-self::*[@attr = 'val']")
    expect_error(gt$css_to_xpath("[attr=val x]"))
})

test_that("man/selectors.Rd structural pseudo-class examples match live translation", {
    gt <- GenericTranslator$new()
    expect_equal(gt$css_to_xpath(":root"),
                 "descendant-or-self::*[not(parent::*)]")
    expect_equal(gt$css_to_xpath(":first-child"),
                 "descendant-or-self::*[count(preceding-sibling::*) = 0]")
    expect_equal(gt$css_to_xpath(":last-child"),
                 "descendant-or-self::*[count(following-sibling::*) = 0]")
    expect_equal(gt$css_to_xpath(":only-child"),
                 "descendant-or-self::*[count(preceding-sibling::*) = 0 and count(following-sibling::*) = 0]")
    expect_equal(gt$css_to_xpath("e:first-of-type"),
                 "descendant-or-self::e[count(preceding-sibling::e) = 0]")
    expect_equal(gt$css_to_xpath("e:last-of-type"),
                 "descendant-or-self::e[count(following-sibling::e) = 0]")
    expect_equal(gt$css_to_xpath("e:only-of-type"),
                 "descendant-or-self::e[count(preceding-sibling::e) = 0 and count(following-sibling::e) = 0]")
    expect_error(gt$css_to_xpath("*:first-of-type"))
    expect_equal(gt$css_to_xpath(":empty"),
                 "descendant-or-self::*[not(*) and not(string-length())]")
    expect_equal(gt$css_to_xpath(":scope"), "self::*")
})

test_that("man/selectors.Rd selector-list pseudo-class examples match live translation", {
    gt <- GenericTranslator$new()
    expect_equal(gt$css_to_xpath(":not(e)"), "descendant-or-self::*[not(self::e)]")
    expect_equal(gt$css_to_xpath(":is(e, f)"),
                 "descendant-or-self::*[self::e or self::f]")
    expect_equal(gt$css_to_xpath(":where(e, f)"),
                 "descendant-or-self::*[self::e or self::f]")
    expect_equal(gt$css_to_xpath(":has(> e)"), "descendant-or-self::*[child::e]")
})

test_that("man/selectors.Rd linguistic/directionality examples match live translation", {
    gt <- GenericTranslator$new()
    expect_equal(gt$css_to_xpath(":lang(en)"), "descendant-or-self::*[lang('en')]")
    expect_equal(gt$css_to_xpath(":dir(ltr)"), "descendant-or-self::*[0]")
})

test_that("man/selectors.Rd link/interaction-state examples match live translation", {
    gt <- GenericTranslator$new()
    ht <- HTMLTranslator$new()
    expect_equal(gt$css_to_xpath(":link"), "descendant-or-self::*[0]")
    expect_equal(gt$css_to_xpath(":visited"), "descendant-or-self::*[0]")
    expect_equal(gt$css_to_xpath(":hover"), "descendant-or-self::*[0]")
    expect_equal(gt$css_to_xpath(":target"), "descendant-or-self::*[0]")

    expect_equal(ht$css_to_xpath(":visited"), "descendant-or-self::*[0]")
    expect_true(grepl("@href", ht$css_to_xpath(":link"), fixed = TRUE))
    expect_true(grepl("@href", ht$css_to_xpath(":any-link"), fixed = TRUE))
})

test_that("man/selectors.Rd namespace examples match live translation", {
    gt <- GenericTranslator$new()
    expect_equal(gt$css_to_xpath("p"), "descendant-or-self::p")
    expect_equal(gt$css_to_xpath("d|p"), "descendant-or-self::d:p")
    expect_equal(gt$css_to_xpath("*|p"),
                 "descendant-or-self::*[local-name() = 'p']")
    expect_equal(gt$css_to_xpath("|p"), "descendant-or-self::p")
})

test_that("man/selectors.Rd HTML form-state pseudo-classes match the documented element set", {
    # The full XPath for these is too long to check in against man/selectors.Rd
    # verbatim (see that page); assert the documented element/attribute facts
    # against the live translator instead.
    ht <- HTMLTranslator$new()

    checked <- ht$css_to_xpath(":checked")
    expect_true(grepl("'option'", checked, fixed = TRUE))
    expect_true(grepl("@checked", checked, fixed = TRUE))
    expect_true(grepl("'checkbox'", checked, fixed = TRUE))
    expect_true(grepl("'radio'", checked, fixed = TRUE))

    read_write <- ht$css_to_xpath(":read-write")
    read_only <- ht$css_to_xpath(":read-only")
    read_write_inner <- sub("^descendant-or-self::\\*\\[", "",
                             sub("\\]$", "", read_write))
    expect_equal(read_only,
                 paste0("descendant-or-self::*[not(", read_write_inner, ")]"))

    expect_true(grepl("@placeholder", ht$css_to_xpath(":placeholder-shown"),
                       fixed = TRUE))
    expect_true(grepl("'fieldset'", ht$css_to_xpath(":enabled"), fixed = TRUE))
    expect_true(grepl("'fieldset'", ht$css_to_xpath(":disabled"), fixed = TRUE))
})
