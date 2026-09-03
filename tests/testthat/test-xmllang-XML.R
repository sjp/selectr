test_that("xml lang function matches correct elements", {
    xmlLangText <- fixture_xmllang()

    skip_if_not_installed("XML")
    xmldoc <- XML::xmlRoot(XML::xmlParse(xmlLangText))
    gt <- GenericTranslator$new()

    pid <- function(selector) {
        xpath <- gt$css_to_xpath(selector)
        items <- XML::getNodeSet(xmldoc, xpath)
        n <- length(items)
        if (!n)
            return(NULL)
        result <- character(n)
        for (i in seq_len(n)) {
            element <- items[[i]]
            tmp <- XML::xmlAttrs(element)["id"]
            if (is.null(tmp))
                tmp <- "nil"
            result[i] <- tmp
        }
        result
    }

    expect_equal(pid(':lang("EN")'), c('first', 'second', 'third', 'fourth'))
    expect_equal(pid(':lang("en-us")'), c('second', 'fourth'))
    expect_equal(pid(':lang(en-nz)'), 'third')
    expect_equal(pid(':lang(fr)'), 'fifth')
    expect_equal(pid(':lang(ru)'), 'sixth')
    expect_equal(pid(":lang('ZH')"), 'eighth')
    expect_equal(pid(':lang(de) :lang(zh)'), 'eighth')
    expect_equal(pid(':lang(en), :lang(zh)'), c('first', 'second', 'third', 'fourth', 'eighth'))
    expect_equal(pid(":lang(es)"), NULL)
    # Wildcard language ranges match the primary subtag and any extension
    expect_equal(pid(':lang(en-*)'), c('first', 'second', 'third', 'fourth'))
    expect_equal(pid(':lang(fr-*)'), 'fifth')
    expect_equal(pid(':lang(es-*)'), NULL)
})
