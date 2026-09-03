test_that("no test file attaches XML or xml2", {
    # library() is not scoped to the test_that() block that calls it: the
    # first file to attach a package leaves it on the search path for the
    # rest of the run, so a later file that forgot to qualify a call
    # would pass in the full suite and fail under testthat::test_file()
    # on its own. Every use of the two suggested packages is written
    # XML:: / xml2:: instead.
    files <- list.files(test_path("."), pattern = "\\.R$", full.names = TRUE)
    attaches <- function(f)
        any(grepl("^\\s*(library|require)\\(", readLines(f)))
    expect_equal(basename(Filter(attaches, files)), character(0))
})
