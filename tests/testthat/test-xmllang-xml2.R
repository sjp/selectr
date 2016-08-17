context("lang-xml2")

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

    expect_that(pid(':lang("EN")'), equals(c('first', 'second', 'third', 'fourth')))
    expect_that(pid(':lang("en-us")'), equals(c('second', 'fourth')))
    expect_that(pid(':lang(en-nz)'), equals('third'))
    expect_that(pid(':lang(fr)'), equals('fifth'))
    expect_that(pid(':lang(ru)'), equals('sixth'))
    expect_that(pid(":lang('ZH')"), equals('eighth'))
    expect_that(pid(':lang(de) :lang(zh)'), equals('eighth'))
    expect_that(pid(':lang(en), :lang(zh)'), equals(c('first', 'second', 'third', 'fourth', 'eighth')))
    expect_that(pid(":lang(es)"), equals(NULL))
})
