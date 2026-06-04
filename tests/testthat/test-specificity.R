context("tokenizer")

test_that("parser creates correct specificity", {
    spec <- function(css) {
        selectors <- parse(css)
        if (length(selectors) != 1)
            stop("More than one result attempting to be parsed.")
        selectors[[1]]$specificity()
    }

    expect_that(spec("*"), equals(rep(0, 3)))
    expect_that(spec(" foo"), equals(c(0, 0, 1)))
    expect_that(spec(":empty"), equals(c(0, 1, 0)))
    expect_that(spec(":before"), equals(c(0, 0, 1)))
    expect_that(spec("*:before"), equals(c(0, 0, 1)))
    expect_that(spec(":nth-child(2)"), equals(c(0, 1, 0)))
    expect_that(spec(".bar"), equals(c(0, 1, 0)))
    expect_that(spec("[baz]"), equals(c(0, 1, 0)))
    expect_that(spec('[baz="4"]'), equals(c(0, 1, 0)))
    expect_that(spec('[baz^="4"]'), equals(c(0, 1, 0)))
    expect_that(spec("#lipsum"), equals(c(1, 0, 0)))

    expect_that(spec(":not(*)"), equals(c(0, 0, 0)))
    expect_that(spec(":not(foo)"), equals(c(0, 0, 1)))
    expect_that(spec(":not(.foo)"), equals(c(0, 1, 0)))
    expect_that(spec(":not([foo])"), equals(c(0, 1, 0)))
    expect_that(spec(":not(:empty)"), equals(c(0, 1, 0)))
    expect_that(spec(":not(#foo)"), equals(c(1, 0, 0)))

    # :not() with multiple arguments - takes max specificity per CSS4
    expect_that(spec(":not(*, foo)"), equals(c(0, 0, 1)))
    expect_that(spec(":not(.foo, .bar)"), equals(c(0, 1, 0)))
    expect_that(spec(":not(.foo, #bar)"), equals(c(1, 0, 0)))
    expect_that(spec(":not(foo, .bar)"), equals(c(0, 1, 0)))
    expect_that(spec(":not(foo, #bar)"), equals(c(1, 0, 0)))
    expect_that(spec(":not(.foo, .bar, .baz)"), equals(c(0, 1, 0)))
    expect_that(spec(":not(#foo, #bar, #baz)"), equals(c(1, 0, 0)))
    expect_that(spec(":not(p, span, div)"), equals(c(0, 0, 1)))
    expect_that(spec(":not([foo], [bar])"), equals(c(0, 1, 0)))
    expect_that(spec(":not(:hover, :visited)"), equals(c(0, 1, 0)))
    expect_that(spec(":not(.foo, [bar], #baz)"), equals(c(1, 0, 0)))

    # Nested :not() - specificity composes through nesting
    expect_that(spec(":not(:not(foo))"), equals(c(0, 0, 1)))
    expect_that(spec(":not(:not(.foo))"), equals(c(0, 1, 0)))
    expect_that(spec(":not(:not(#foo))"), equals(c(1, 0, 0)))
    expect_that(spec(":is(:not(.foo), bar)"), equals(c(0, 1, 0)))

    # :not() with multiple arguments in combinations
    expect_that(spec("div:not(.foo, #bar)"), equals(c(1, 0, 1)))
    expect_that(spec("p:not(span, .foo)"), equals(c(0, 1, 1)))
    expect_that(spec("#main:not(.foo, .bar)"), equals(c(1, 1, 0)))
    expect_that(spec(".test:not(#foo, [bar])"), equals(c(1, 1, 0)))

    expect_that(spec(":is(.foo, #bar)"), equals(c(1, 0, 0)))
    expect_that(spec(":is(:hover, :visited)"), equals(c(0, 1, 0)))
    expect_that(spec(":matches(.foo, #bar)"), equals(c(1, 0, 0)))
    expect_that(spec(":matches(:hover, :visited)"), equals(c(0, 1, 0)))

    # :where() always has zero specificity
    expect_that(spec(":where(.foo, #bar)"), equals(c(0, 0, 0)))
    expect_that(spec(":where(:hover, :visited)"), equals(c(0, 0, 0)))
    expect_that(spec("div:where(.foo, #bar)"), equals(c(0, 0, 1)))
    expect_that(spec("p:where(span, .foo)"), equals(c(0, 0, 1)))
    expect_that(spec("#main:where(.foo, .bar)"), equals(c(1, 0, 0)))
    expect_that(spec(".test:where(#foo, [bar])"), equals(c(0, 1, 0)))

    # :has() takes the maximum specificity from its argument list
    expect_that(spec(":has(.foo, #bar)"), equals(c(1, 0, 0)))
    expect_that(spec(":has(:hover, :visited)"), equals(c(0, 1, 0)))
    expect_that(spec("div:has(.foo, #bar)"), equals(c(1, 0, 1)))
    expect_that(spec("p:has(span, .foo)"), equals(c(0, 1, 1)))
    expect_that(spec("#main:has(.foo, .bar)"), equals(c(1, 1, 0)))
    expect_that(spec(".test:has(#foo, [bar])"), equals(c(1, 1, 0)))

    # single-argument :has()
    expect_that(spec(":has(.foo)"), equals(c(0, 1, 0)))
    expect_that(spec("e:has(img)"), equals(c(0, 0, 2)))

    # leading combinators contribute no specificity
    expect_that(spec("e:has(> img)"), equals(c(0, 0, 2)))
    expect_that(spec("e:has(~ .foo)"), equals(c(0, 1, 1)))
    expect_that(spec("e:has(> .foo, ~ #bar)"), equals(c(1, 0, 1)))

    # complex selectors in arguments sum across their compounds
    expect_that(spec(":is(a b)"), equals(c(0, 0, 2)))
    expect_that(spec(":not(a > b, #c)"), equals(c(1, 0, 0)))
    expect_that(spec("e:has(a b.foo)"), equals(c(0, 1, 3)))
    expect_that(spec(":where(a b)"), equals(c(0, 0, 0)))

    # single-argument :is() and a non-universal base compound compose
    expect_that(spec(":is(.foo)"), equals(c(0, 1, 0)))
    expect_that(spec("div:is(.foo)"), equals(c(0, 1, 1)))

    expect_that(spec("foo:empty"), equals(c(0, 1, 1)))
    expect_that(spec("foo:before"), equals(c(0, 0, 2)))
    expect_that(spec("foo::before"), equals(c(0, 0, 2)))
    expect_that(spec("foo:empty::before"), equals(c(0, 1, 2)))

    # combinations
    expect_that(spec("* foo"), equals(c(0, 0, 1)))
    expect_that(spec("foo :empty"), equals(c(0, 1, 1)))
    expect_that(spec(":empty :before"), equals(c(0, 1, 1)))
    expect_that(spec(".bar [baz]"), equals(c(0, 2, 0)))
    expect_that(spec('[baz] [baz="4"]'), equals(c(0, 2, 0)))
    expect_that(spec('[baz="4"] [baz^="4"]'), equals(c(0, 2, 0)))
    expect_that(spec('[baz^="4"] #lipsum'), equals(c(1, 1, 0)))
})
