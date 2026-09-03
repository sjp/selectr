test_that("selection works correctly on a shakespearean document", {
    HTML_SHAKESPEARE <- fixture_html_shakespeare()

    skip_if_not_installed("xml2")
    document <- xml2::read_xml(HTML_SHAKESPEARE)
    xml2::xml_ns_strip(document)
    body <- xml2::xml_find_first(document, "//body")
    gt <- GenericTranslator$new()

    count <- function(selector) {
        xpath <- gt$css_to_xpath(selector)
        results <- xml2::xml_find_all(body, xpath)
        length(results)
    }

    # Data borrowed from http://mootools.net/slickspeed/

    ## Changed from original; probably because I'm only
    ## searching the body.
    #expect_equal(count('*'), 252)
    expect_equal(count('*'), 246)
    expect_equal(count('div:only-child'), 22) # ?
    expect_equal(count('div:nth-child(even)'), 106)
    expect_equal(count('div:nth-child(2n)'), 106)
    expect_equal(count('div:nth-child(odd)'), 137)
    expect_equal(count('div:nth-child(2n+1)'), 137)
    expect_equal(count('div:nth-child(n)'), 243)
    expect_equal(count('div:last-child'), 53)
    expect_equal(count('div:first-child'), 51)
    expect_equal(count('div > div'), 242)
    expect_equal(count('div + div'), 190)
    expect_equal(count('div ~ div'), 190)
    expect_equal(count('body'), 1)
    expect_equal(count('body div'), 243)
    expect_equal(count('div'), 243)
    expect_equal(count('div div'), 242)
    expect_equal(count('div div div'), 241)
    expect_equal(count('div, div, div'), 243)
    expect_equal(count('div, a, span'), 243)
    expect_equal(count('.dialog'), 51)
    expect_equal(count('div.dialog'), 51)
    expect_equal(count('div .dialog'), 51)
    expect_equal(count('div.character, div.dialog'), 99)
    expect_equal(count('div.direction.dialog'), 0)
    expect_equal(count('div.dialog.direction'), 0)
    expect_equal(count('div.dialog.scene'), 1)
    expect_equal(count('div.scene.scene'), 1)
    expect_equal(count('div.scene .scene'), 0)
    expect_equal(count('div.direction .dialog '), 0)
    expect_equal(count('div .dialog .direction'), 4)
    expect_equal(count('div.dialog .dialog .direction'), 4)
    expect_equal(count('#speech5'), 1)
    expect_equal(count('div#speech5'), 1)
    expect_equal(count('div #speech5'), 1)
    expect_equal(count('div.scene div.dialog'), 49)
    expect_equal(count('div#scene1 div.dialog div'), 142)
    expect_equal(count('#scene1 #speech1'), 1)
    expect_equal(count('div[class]'), 103)
    expect_equal(count('div[class=dialog]'), 50)
    expect_equal(count('div[class^=dia]'), 51)
    expect_equal(count('div[class$=log]'), 50)
    expect_equal(count('div[class*=sce]'), 1)
    expect_equal(count('div[class|=dialog]'), 50) # ? Seems right
    expect_equal(count('div[class~=dialog]'), 51) # ? Seems right
})
