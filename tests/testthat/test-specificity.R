test_that("parser creates correct specificity", {
    spec <- function(css) {
        selectors <- parse(css)
        if (length(selectors) != 1)
            stop("More than one result attempting to be parsed.")
        selectors[[1]]$specificity()
    }

    expect_equal(spec("*"), rep(0, 3))
    expect_equal(spec(" foo"), c(0, 0, 1))
    expect_equal(spec(":empty"), c(0, 1, 0))
    expect_equal(spec(":before"), c(0, 0, 1))
    expect_equal(spec("*:before"), c(0, 0, 1))
    expect_equal(spec(":nth-child(2)"), c(0, 1, 0))
    expect_equal(spec(".bar"), c(0, 1, 0))
    expect_equal(spec("[baz]"), c(0, 1, 0))
    expect_equal(spec('[baz="4"]'), c(0, 1, 0))
    expect_equal(spec('[baz^="4"]'), c(0, 1, 0))
    expect_equal(spec("#lipsum"), c(1, 0, 0))

    expect_equal(spec(":not(*)"), c(0, 0, 0))
    expect_equal(spec(":not(foo)"), c(0, 0, 1))
    expect_equal(spec(":not(.foo)"), c(0, 1, 0))
    expect_equal(spec(":not([foo])"), c(0, 1, 0))
    expect_equal(spec(":not(:empty)"), c(0, 1, 0))
    expect_equal(spec(":not(#foo)"), c(1, 0, 0))

    # :not() with multiple arguments - takes max specificity per CSS4
    expect_equal(spec(":not(*, foo)"), c(0, 0, 1))
    expect_equal(spec(":not(.foo, .bar)"), c(0, 1, 0))
    expect_equal(spec(":not(.foo, #bar)"), c(1, 0, 0))
    expect_equal(spec(":not(foo, .bar)"), c(0, 1, 0))
    expect_equal(spec(":not(foo, #bar)"), c(1, 0, 0))
    expect_equal(spec(":not(.foo, .bar, .baz)"), c(0, 1, 0))
    expect_equal(spec(":not(#foo, #bar, #baz)"), c(1, 0, 0))
    expect_equal(spec(":not(p, span, div)"), c(0, 0, 1))
    expect_equal(spec(":not([foo], [bar])"), c(0, 1, 0))
    expect_equal(spec(":not(:hover, :visited)"), c(0, 1, 0))
    expect_equal(spec(":not(.foo, [bar], #baz)"), c(1, 0, 0))

    # Nested :not() - specificity composes through nesting
    expect_equal(spec(":not(:not(foo))"), c(0, 0, 1))
    expect_equal(spec(":not(:not(.foo))"), c(0, 1, 0))
    expect_equal(spec(":not(:not(#foo))"), c(1, 0, 0))
    expect_equal(spec(":is(:not(.foo), bar)"), c(0, 1, 0))

    # :not() with multiple arguments in combinations
    expect_equal(spec("div:not(.foo, #bar)"), c(1, 0, 1))
    expect_equal(spec("p:not(span, .foo)"), c(0, 1, 1))
    expect_equal(spec("#main:not(.foo, .bar)"), c(1, 1, 0))
    expect_equal(spec(".test:not(#foo, [bar])"), c(1, 1, 0))

    expect_equal(spec(":is(.foo, #bar)"), c(1, 0, 0))
    expect_equal(spec(":is(:hover, :visited)"), c(0, 1, 0))
    expect_equal(spec(":matches(.foo, #bar)"), c(1, 0, 0))
    expect_equal(spec(":matches(:hover, :visited)"), c(0, 1, 0))

    # :where() always has zero specificity
    expect_equal(spec(":where(.foo, #bar)"), c(0, 0, 0))
    expect_equal(spec(":where(:hover, :visited)"), c(0, 0, 0))
    expect_equal(spec("div:where(.foo, #bar)"), c(0, 0, 1))
    expect_equal(spec("p:where(span, .foo)"), c(0, 0, 1))
    expect_equal(spec("#main:where(.foo, .bar)"), c(1, 0, 0))
    expect_equal(spec(".test:where(#foo, [bar])"), c(0, 1, 0))

    # :has() takes the maximum specificity from its argument list
    expect_equal(spec(":has(.foo, #bar)"), c(1, 0, 0))
    expect_equal(spec(":has(:hover, :visited)"), c(0, 1, 0))
    expect_equal(spec("div:has(.foo, #bar)"), c(1, 0, 1))
    expect_equal(spec("p:has(span, .foo)"), c(0, 1, 1))
    expect_equal(spec("#main:has(.foo, .bar)"), c(1, 1, 0))
    expect_equal(spec(".test:has(#foo, [bar])"), c(1, 1, 0))

    # single-argument :has()
    expect_equal(spec(":has(.foo)"), c(0, 1, 0))
    expect_equal(spec("e:has(img)"), c(0, 0, 2))

    # leading combinators contribute no specificity
    expect_equal(spec("e:has(> img)"), c(0, 0, 2))
    expect_equal(spec("e:has(~ .foo)"), c(0, 1, 1))
    expect_equal(spec("e:has(> .foo, ~ #bar)"), c(1, 0, 1))

    # complex selectors in arguments sum across their compounds
    expect_equal(spec(":is(a b)"), c(0, 0, 2))
    expect_equal(spec(":not(a > b, #c)"), c(1, 0, 0))
    expect_equal(spec("e:has(a b.foo)"), c(0, 1, 3))
    expect_equal(spec(":where(a b)"), c(0, 0, 0))

    # an empty forgiving list has no argument to take specificity from
    expect_equal(spec(":is()"), c(0, 0, 0))
    expect_equal(spec("div.foo:is()"), c(0, 1, 1))
    expect_equal(spec("div:where()"), c(0, 0, 1))

    # single-argument :is() and a non-universal base compound compose
    expect_equal(spec(":is(.foo)"), c(0, 1, 0))
    expect_equal(spec("div:is(.foo)"), c(0, 1, 1))

    expect_equal(spec("foo:empty"), c(0, 1, 1))
    expect_equal(spec("foo:before"), c(0, 0, 2))
    expect_equal(spec("foo::before"), c(0, 0, 2))
    expect_equal(spec("foo:empty::before"), c(0, 1, 2))

    # combinations
    expect_equal(spec("* foo"), c(0, 0, 1))
    expect_equal(spec("foo :empty"), c(0, 1, 1))
    expect_equal(spec(":empty :before"), c(0, 1, 1))
    expect_equal(spec(".bar [baz]"), c(0, 2, 0))
    expect_equal(spec('[baz] [baz="4"]'), c(0, 2, 0))
    expect_equal(spec('[baz="4"] [baz^="4"]'), c(0, 2, 0))
    expect_equal(spec('[baz^="4"] #lipsum'), c(1, 1, 0))

    # :nth-child(An+B of S) / :nth-last-child(An+B of S) add the most
    # specific complex selector in S, per Selectors Level 4
    expect_equal(spec("li:nth-child(2 of .a, #b)"), c(1, 1, 1))
    expect_equal(spec(":nth-last-child(1 of #x)"), c(1, 1, 0))
    expect_equal(spec(":nth-child(2 of .a)"), c(0, 2, 0))
    expect_equal(spec("#id:nth-child(2 of .a)"), c(1, 2, 0))
})
