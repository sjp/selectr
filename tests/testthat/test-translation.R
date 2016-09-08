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
    expect_that(xpath('e[foo!="bar"]'), equals("e[not(@foo) or @foo != 'bar']"))
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
                equals("e[count(preceding-sibling::*) = 0]"))
    expect_that(xpath('e:nth-child(3n+2)'),
                equals("e[count(preceding-sibling::*) >= 1 and (count(preceding-sibling::*) +2) mod 3 = 0]"))
    expect_that(xpath('e:nth-child(3n-2)'),
                equals("e[count(preceding-sibling::*) mod 3 = 0]"))
    expect_that(xpath('e:nth-child(-n+6)'),
                equals("e[count(preceding-sibling::*) <= 5]"))
    expect_that(xpath('e:nth-last-child(1)'),
                equals("e[count(following-sibling::*) = 0]"))
    expect_that(xpath('e:nth-last-child(2n)'),
                equals("e[(count(following-sibling::*) +1) mod 2 = 0]"))
    expect_that(xpath('e:nth-last-child(2n+1)'),
                equals("e[count(following-sibling::*) mod 2 = 0]"))
    expect_that(xpath('e:nth-last-child(2n+2)'),
                equals("e[count(following-sibling::*) >= 1 and (count(following-sibling::*) +1) mod 2 = 0]"))
    expect_that(xpath('e:nth-last-child(3n+1)'),
                equals("e[count(following-sibling::*) mod 3 = 0]"))
    expect_that(xpath('e:nth-last-child(-n+2)'),
                equals("e[count(following-sibling::*) <= 1]"))
    expect_that(xpath('e:nth-of-type(1)'),
                equals("e[count(preceding-sibling::e) = 0]"))
    expect_that(xpath('e:nth-last-of-type(1)'),
                equals("e[count(following-sibling::e) = 0]"))
    expect_that(xpath('div e:nth-last-of-type(1) .aclass'),
                equals("div/descendant::e[count(following-sibling::e) = 0]/descendant::*[@class and contains(concat(' ', normalize-space(@class), ' '), ' aclass ')]"))
    expect_that(xpath('e:first-child'),
                equals("e[count(preceding-sibling::*) = 0]"))
    expect_that(xpath('e:last-child'),
                equals("e[count(following-sibling::*) = 0]"))
    expect_that(xpath('e:first-of-type'),
                equals("e[count(preceding-sibling::e) = 0]"))
    expect_that(xpath('e:last-of-type'),
                equals("e[count(following-sibling::e) = 0]"))
    expect_that(xpath('e:only-child'),
                equals("e[count(parent::*/child::*) = 1]"))
    expect_that(xpath('e:only-of-type'),
                equals("e[count(parent::*/child::e) = 1]"))
    expect_that(xpath('e:empty'),
                equals("e[not(*) and not(string-length())]"))
    expect_that(xpath('e:EmPTY'),
                equals("e[not(*) and not(string-length())]"))
    expect_that(xpath('e:root'),
                equals("e[not(parent::*)]"))
    expect_that(xpath('e:hover'),
                equals("e[0]")) #never matches
    expect_that(xpath('e:contains("foo")'),
                equals("e[contains(., 'foo')]"))
    expect_that(xpath('e:ConTains(foo)'),
                equals("e[contains(., 'foo')]"))
    expect_that(xpath('e.warning'),
                equals("e[@class and contains(concat(' ', normalize-space(@class), ' '), ' warning ')]"))
    expect_that(xpath('e#myid'),
                equals("e[@id = 'myid']"))
    expect_that(xpath('e:not(:nth-child(odd))'),
                equals("e[not(count(preceding-sibling::*) mod 2 = 0)]"))
    expect_that(xpath('e:nOT(*)'),
                equals("e[0]")) # never matches
    expect_that(xpath('e f'),
                equals("e/descendant::f"))
    expect_that(xpath('e > f'),
                equals("e/f"))
    expect_that(xpath('e + f'),
                equals("e/following-sibling::*[name() = 'f' and (position() = 1)]"))
    expect_that(xpath('e ~ f'),
                equals("e/following-sibling::f"))
    expect_that(xpath('e ~ f:nth-child(3)'),
                equals("e/following-sibling::f[count(preceding-sibling::*) = 2]"))
    expect_that(xpath('div#container p'),
                equals("div[@id = 'container']/descendant::p"))

    # expect that the following do nothing for the generic translator
    expect_that(xpath('a:link'), equals("a[0]"))
    expect_that(xpath('a:visited'), equals("a[0]"))
    expect_that(xpath('a:hover'), equals("a[0]"))
    expect_that(xpath('a:active'), equals("a[0]"))
    expect_that(xpath('a:focus'), equals("a[0]"))
    expect_that(xpath('a:target'), equals("a[0]"))
    expect_that(xpath('a:enabled'), equals("a[0]"))
    expect_that(xpath('a:disabled'), equals("a[0]"))
    expect_that(xpath('a:checked'), equals("a[0]"))

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
})
