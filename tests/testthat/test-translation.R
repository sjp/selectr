context("translation")

test_that("translation from parsed objects to XPath works", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    expect_that(xpath("*"), equals("*"))        
    expect_that(xpath("e"), equals("e"))
    expect_that(xpath("*|e"), equals("e"))
    expect_that(xpath("e|f"), equals("e:f"))
    expect_that(xpath("e[foo]"), equals("e[@foo]"))
    expect_that(xpath("e[foo|bar]"), equals("e[@foo:bar]"))
    expect_that(xpath('e[foo="bar"]'), equals("e[@foo = 'bar']"))
    expect_that(xpath('e[foo~="bar"]'),
                equals("e[@foo and contains(concat(' ', normalize-space(@foo), ' '), ' bar ')]"))
    expect_that(xpath('e[foo^="bar"]'),
                equals("e[@foo and starts-with(@foo, 'bar')]"))
    expect_that(xpath('e[foo$="bar"]'),
                equals("e[@foo and substring(@foo, string-length(@foo)-2) = 'bar']"))
    expect_that(xpath('e[foo*="bar"]'),
                equals("e[@foo and contains(@foo, 'bar')]"))
    expect_that(xpath('e[hreflang|="en"]'),
                equals("e[@hreflang and (@hreflang = 'en' or starts-with(@hreflang, 'en-'))]"))
    expect_that(xpath('e:nth-child(1)'),
                equals("*/*[name() = 'e' and (position() = 1)]"))
    expect_that(xpath('e:nth-child(3n+2)'),
                equals("*/*[name() = 'e' and ((position() -2) mod 3 = 0 and position() >= 2)]"))
    expect_that(xpath('e:nth-child(3n-2)'),
                equals("*/*[name() = 'e' and ((position() +2) mod 3 = 0)]"))
    expect_that(xpath('e:nth-child(-n+6)'),
                equals("*/*[name() = 'e' and ((position() -6) mod -1 = 0 and position() <= 6)]"))
    expect_that(xpath('e:nth-last-child(1)'),
                equals("*/*[name() = 'e' and (position() = last())]"))
    expect_that(xpath('e:nth-last-child(2n)'),
                equals("*/*[name() = 'e' and ((last() - position() +1) mod 2 = 0 and (position() <= last() +1))]"))
    expect_that(xpath('e:nth-last-child(2n+2)'),
                equals("*/*[name() = 'e' and ((last() - position() -2 +1) mod 2 = 0 and position() <= (last() -2 +1))]"))
    expect_that(xpath('e:nth-of-type(1)'),
                equals("*/e[position() = 1]"))
    expect_that(xpath('e:nth-last-of-type(1)'),
                equals("*/e[position() = last()]"))
    expect_that(xpath('div e:nth-last-of-type(1) .aclass'),
                equals("div/descendant-or-self::*/e[position() = last()]/descendant-or-self::*/*[@class and contains(concat(' ', normalize-space(@class), ' '), ' aclass ')]"))
    expect_that(xpath('e:first-child'),
                equals("*/*[name() = 'e' and (position() = 1)]"))
    expect_that(xpath('e:last-child'),
                equals("*/*[name() = 'e' and (position() = last())]"))
    expect_that(xpath('e:first-of-type'),
                equals("*/e[position() = 1]"))
    expect_that(xpath('e:last-of-type'),
                equals("*/e[position() = last()]"))
    expect_that(xpath('e:only-child'),
                equals("*/*[name() = 'e' and (last() = 1)]"))
    expect_that(xpath('e:only-of-type'),
                equals("e[last() = 1]"))            
    expect_that(xpath('e:empty'),
                equals("e[not(*) and not(string-length())]"))
    expect_that(xpath('e:EmPTY'),
                equals("e[not(*) and not(string-length())]"))
    expect_that(xpath('e:root'),
                equals("e[not(parent::*)]"))
    expect_that(xpath('e:hover'),
                equals("e[0]")) #never matches
    expect_that(xpath('e:contains("foo")'),
                equals("e[contains(string(.), 'foo')]"))
    expect_that(xpath('e:ConTains(foo)'),
                equals("e[contains(string(.), 'foo')]"))
    expect_that(xpath('e.warning'),
                equals("e[@class and contains(concat(' ', normalize-space(@class), ' '), ' warning ')]"))
    expect_that(xpath('e#myid'),
                equals("e[@id = 'myid']"))
    expect_that(xpath('e:not(:nth-child(odd))'),
                equals("e[not((position() -1) mod 2 = 0 and position() >= 1)]"))
    expect_that(xpath('e:nOT(*)'),
                equals("e[0]")) # never matches        
    expect_that(xpath('e f'),
                equals("e/descendant-or-self::*/f"))
    expect_that(xpath('e > f'),
                equals("e/f"))
    expect_that(xpath('e + f'),
                equals("e/following-sibling::*[name() = 'f' and (position() = 1)]"))
    expect_that(xpath('e ~ f'),
                equals("e/following-sibling::f"))
    expect_that(xpath('div#container p'),
                equals("div[@id = 'container']/descendant-or-self::*/p"))

    # Invalid characters in XPath element names

    if (localeToCharset()[1] == "UTF-8") {
        expect_that(xpath('di\ua0v'),
                    equals("*[name() = 'di v']")) # div\ua0v
        expect_that(xpath('[h\ua0ref]'),
                    equals("*[attribute::*[name() = 'h ref']]")) # h\ua0ref
    }
    expect_that(xpath('di\\[v'),
                equals("*[name() = 'di[v']"))
    expect_that(xpath('[h\\]ref]'),
                equals("*[attribute::*[name() = 'h]ref']]"))

    #expect_that(xpath(":fİrst-child"), throws_error())
    #expect_that(xpath(":first-of-type"), throws_error())
    #expect_that(xpath(":only-of-type"), throws_error())
    #expect_that(xpath(":last-of-type"), throws_error())
    #expect_that(xpath(":nth-of-type(1)"), throws_error())
    #expect_that(xpath(":nth-last-of-type(1)"), throws_error())
    #expect_that(xpath(":nth-child(n-)"), throws_error())
    #expect_that(xpath(":after"), throws_error())
    #expect_that(xpath(":lorem-ipsum"), throws_error())
    #expect_that(xpath(":lorem(ipsum)"), throws_error())
    #expect_that(xpath("::lorem-ipsum"), throws_error())
    #expect_that({gt <- GenericTranslator$new() ; gt$css_to_xpath(4)}, throws_error())
    #expect_that({gt <- GenericTranslator$new() ; gt$selector_to_xpath("foo") }, throws_error())
})
