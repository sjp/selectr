context("pseudo")

test_that("parser parses canonical pseudo element expressions", {
    parse_pseudo <- function(css) {
        selectors <- lapply(css, function(x) parse(x))
        n <- length(selectors)
        results <- list()
        for (i in seq_len(n)) {
            selector <- selectors[[i]]
            if (is.list(selector)) {
                results[[i]] <- lapply(selector,
                                       function(x) {
                                           el <- x
                                           pseudo <- x$pseudo_element
                                           el$pseudo_element <- NULL
                                           list(el$repr(), pseudo)
                                       })
            } else {
                pseudo <- selector$pseudo_element
                selector$pseudo_element <- NULL
                results[[i]] <- list(list(selector$repr(), pseudo))
            }
        }
        if (n)
            results[[seq_len(n)]]
        else
            results
    }

    parse_one <- function(css) {
        result <- parse_pseudo(css)
        #if (length(result) != 1)
        #    stop("More than one result attempting to be parsed.")
        result[[1]]
    }

    expect_that(parse_one("foo"),
                equals(list("Element[foo]", NULL)))
    expect_that(parse_one("*"),
                equals(list("Element[*]", NULL)))
    expect_that(parse_one(":empty"),
                equals(list("Pseudo[Element[*]:empty]", NULL)))

    # Special cases for CSS 2.1 pseudo-elements
    expect_that(parse_one(":BEfore"),
                equals(list("Element[*]", "before")))
    expect_that(parse_one(":aftER"),
                equals(list("Element[*]", "after")))
    expect_that(parse_one(":First-Line"),
                equals(list("Element[*]", "first-line")))
    expect_that(parse_one(":First-Letter"),
                equals(list("Element[*]", "first-letter")))

    expect_that(parse_one("::befoRE"),
                equals(list("Element[*]", "before")))
    expect_that(parse_one("::AFter"),
                equals(list("Element[*]", "after")))
    expect_that(parse_one("::firsT-linE"),
                equals(list("Element[*]", "first-line")))
    expect_that(parse_one("::firsT-letteR"),
                equals(list("Element[*]", "first-letter")))

    expect_that(parse_one("::Selection"),
                equals(list("Element[*]", "selection")))
    expect_that(parse_one("foo:after"),
                equals(list("Element[foo]", "after")))
    expect_that(parse_one("foo::selection"),
                equals(list("Element[foo]", "selection")))
    expect_that(parse_one("lorem#ipsum ~ a#b.c[href]:empty::selection"),
                equals(list("CombinedSelector[Hash[Element[lorem]#ipsum] ~ Pseudo[Attrib[Class[Hash[Element[a]#b].c][href]]:empty]]", "selection")))

    expect_that(parse_pseudo("foo:before, bar, baz:after"),
                equals(list(list("Element[foo]", "before"),
                            list("Element[bar]", NULL),
                            list("Element[baz]", "after"))))
})
