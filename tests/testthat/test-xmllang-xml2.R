test_that("xml lang function matches correct elements", {
    xmlLangText <- paste0('<test>',
                          '<a id="first" xml:lang="en">a</a>',
                          '<b id="second" xml:lang="en-US">b</b>',
                          '<c id="third" xml:lang="en-Nz">c</c>',
                          '<d id="fourth" xml:lang="En-us">d</d>',
                          '<e id="fifth" xml:lang="fr">e</e>',
                          '<f id="sixth" xml:lang="ru">f</f>',
                          '<g id="seventh" xml:lang="de"><h id="eighth" xml:lang="zh" /></g>',
                          '</test>')

    skip_if_not_installed("xml2")
    library(xml2)
    xmldoc <- read_xml(xmlLangText)
    gt <- GenericTranslator$new()

    pid <- function(selector) {
        xpath <- gt$css_to_xpath(selector)
        items <- xml_find_all(xmldoc, xpath)
        n <- length(items)
        if (!n)
            return(NULL)
        result <- character(n)
        for (i in seq_len(n)) {
            element <- items[[i]]
            tmp <- xml_attrs(element)["id"]
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
