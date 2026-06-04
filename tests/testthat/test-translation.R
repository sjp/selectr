context("translation")

test_that("translation from parsed objects to XPath works", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    expect_that(xpath("*"), equals("*"))
    expect_that(xpath("e"), equals("e"))
    expect_that(xpath("*|e"), equals("*[(local-name() = 'e')]"))
    expect_that(xpath("|e"), equals("e"))
    expect_that(xpath("e|f"), equals("e:f"))
    expect_that(xpath("e[foo]"), equals("e[(@foo)]"))
    expect_that(xpath("e[foo|bar]"), equals("e[(@foo:bar)]"))
    expect_that(xpath('e[foo="bar"]'), equals("e[(@foo = 'bar')]"))
    expect_that(xpath("e[foo='(test)']"), equals("e[(@foo = '(test)')]"))
    expect_that(xpath('e[foo="(test)"]'), equals("e[(@foo = '(test)')]"))
    expect_that(xpath("e[foo='(abc)']"), equals("e[(@foo = '(abc)')]"))
    expect_that(xpath("e[foo='(e2e)']"), equals("e[(@foo = '(e2e)')]"))
    expect_that(xpath('e[foo="(e2e)"]'), equals("e[(@foo = '(e2e)')]"))
    expect_that(xpath("e[foo='(123)']"), equals("e[(@foo = '(123)')]"))
    expect_that(xpath("e[foo='(12345)']"), equals("e[(@foo = '(12345)')]"))
    # Six hex digits (max for CSS unicode escape)
    expect_that(xpath("e[foo='(abcdef)']"), equals("e[(@foo = '(abcdef)')]"))
    expect_that(xpath("e[foo='(123456)']"), equals("e[(@foo = '(123456)')]"))
    # Seven hex digits (exceeds max, so not unicode escape required)
    expect_that(xpath("e[foo='(1234567)']"), equals("e[(@foo = '(1234567)')]"))
    expect_that(xpath("e[foo='(AbCdEf)']"), equals("e[(@foo = '(AbCdEf)')]"))
    expect_that(xpath("e[foo='(E2E)']"), equals("e[(@foo = '(E2E)')]"))
    expect_that(xpath("e[foo='(o2o)']"), equals("e[(@foo = '(o2o)')]"))
    expect_that(xpath('e[foo="(o2o)"]'), equals("e[(@foo = '(o2o)')]"))
    expect_that(xpath("e[foo='(xyz)']"), equals("e[(@foo = '(xyz)')]"))
    expect_that(xpath("e[foo='(test123)']"), equals("e[(@foo = '(test123)')]"))
    expect_that(xpath("e[foo='(abc)(def)']"), equals("e[(@foo = '(abc)(def)')]"))
    expect_that(xpath("e[foo='(abc )']"), equals("e[(@foo = '(abc )')]"))
    # Unicode escapes are decoded to the characters they represent,
    # in idents, hashes, and strings alike
    expect_that(xpath("#\\31 23"), equals("*[(@id = '123')]"))
    expect_that(xpath("\\31 23"), equals("*[(name() = '123')]"))
    expect_that(xpath("[\\31 23]"),
                equals("*[(attribute::*[name() = '123'])]"))
    expect_that(xpath("e[foo='\\31 23']"), equals("e[(@foo = '123')]"))
    expect_that(xpath("e[foo='x\\79 z']"), equals("e[(@foo = 'xyz')]"))
    expect_that(xpath('e[foo~="bar"]'),
                equals("e[(@foo and contains(concat(' ', normalize-space(@foo), ' '), ' bar '))]"))
    expect_that(xpath('e[foo^="bar"]'),
                equals("e[(@foo and starts-with(@foo, 'bar'))]"))
    expect_that(xpath('e[foo$="bar"]'),
                equals("e[(@foo and substring(@foo, string-length(@foo)-2) = 'bar')]"))
    expect_that(xpath('e[foo*="bar"]'),
                equals("e[(@foo and contains(@foo, 'bar'))]"))
    expect_that(xpath('e[hreflang|="en"]'),
                equals("e[(@hreflang and (@hreflang = 'en' or starts-with(@hreflang, 'en-')))]"))
    # CSS Selectors Level 4 case-sensitivity flags
    lower_foo <- paste0("translate(@foo, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',",
                        " 'abcdefghijklmnopqrstuvwxyz')")
    expect_that(xpath('e[foo="Bar" i]'),
                equals(paste0("e[(", lower_foo, " = 'bar')]")))
    expect_that(xpath('e[foo^="Bar" i]'),
                equals(paste0("e[(", lower_foo, " and starts-with(",
                              lower_foo, ", 'bar'))]")))
    expect_that(xpath('e[foo$="Bar" i]'),
                equals(paste0("e[(", lower_foo, " and substring(",
                              lower_foo, ", string-length(",
                              lower_foo, ")-2) = 'bar')]")))
    expect_that(xpath('e[foo*="Bar" i]'),
                equals(paste0("e[(", lower_foo, " and contains(",
                              lower_foo, ", 'bar'))]")))
    expect_that(xpath('e[foo~="Bar" i]'),
                equals(paste0("e[(", lower_foo,
                              " and contains(concat(' ', normalize-space(",
                              lower_foo, "), ' '), ' bar '))]")))
    expect_that(xpath('e[foo|="Bar" i]'),
                equals(paste0("e[(", lower_foo, " and (",
                              lower_foo, " = 'bar' or starts-with(",
                              lower_foo, ", 'bar-')))]")))
    # The 'i' flag is ASCII case-insensitive: non-ASCII characters such
    # as 'É' are left alone
    expect_that(xpath("e[foo='\\C9 x' i]"),
                equals(paste0("e[(", lower_foo, " = '\uC9x')]")))
    # An empty value cannot differ by case, so it keeps the exact
    # (existence-preserving) translation
    expect_that(xpath('e[foo="" i]'), equals("e[(@foo = '')]"))
    # The 's' flag requests the default case-sensitive matching
    expect_that(xpath('e[foo="Bar" s]'), equals("e[(@foo = 'Bar')]"))
    expect_that(xpath('e[foo^="Bar" s]'),
                equals("e[(@foo and starts-with(@foo, 'Bar'))]"))
    expect_that(xpath('e:nth-child(1)'),
                equals("e[(count(preceding-sibling::*) = 0)]"))
    expect_that(xpath('e:nth-child(3n+2)'),
                equals("e[(count(preceding-sibling::*) >= 1 and (count(preceding-sibling::*) +2) mod 3 = 0)]"))
    expect_that(xpath('e:nth-child(3n-2)'),
                equals("e[(count(preceding-sibling::*) mod 3 = 0)]"))
    expect_that(xpath('e:nth-child(-n+6)'),
                equals("e[(count(preceding-sibling::*) <= 5)]"))
    expect_that(xpath('e:nth-last-child(1)'),
                equals("e[(count(following-sibling::*) = 0)]"))
    expect_that(xpath('e:nth-last-child(2n)'),
                equals("e[((count(following-sibling::*) +1) mod 2 = 0)]"))
    expect_that(xpath('e:nth-last-child(2n+1)'),
                equals("e[(count(following-sibling::*) mod 2 = 0)]"))
    expect_that(xpath('e:nth-last-child(2n+2)'),
                equals("e[(count(following-sibling::*) >= 1 and (count(following-sibling::*) +1) mod 2 = 0)]"))
    expect_that(xpath('e:nth-last-child(3n+1)'),
                equals("e[(count(following-sibling::*) mod 3 = 0)]"))
    expect_that(xpath('e:nth-last-child(-n+2)'),
                equals("e[(count(following-sibling::*) <= 1)]"))
    expect_that(xpath('e:nth-of-type(1)'),
                equals("e[(count(preceding-sibling::e) = 0)]"))
    expect_that(xpath('e:nth-last-of-type(1)'),
                equals("e[(count(following-sibling::e) = 0)]"))
    expect_that(xpath('div e:nth-last-of-type(1) .aclass'),
                equals("div//e[(count(following-sibling::e) = 0)]//*[(@class and contains(concat(' ', normalize-space(@class), ' '), ' aclass '))]"))
    expect_that(xpath('e:first-child'),
                equals("e[(count(preceding-sibling::*) = 0)]"))
    expect_that(xpath('e:last-child'),
                equals("e[(count(following-sibling::*) = 0)]"))
    expect_that(xpath('e:first-of-type'),
                equals("e[(count(preceding-sibling::e) = 0)]"))
    expect_that(xpath('e:last-of-type'),
                equals("e[(count(following-sibling::e) = 0)]"))
    expect_that(xpath('e:only-child'),
                equals("e[(count(parent::*/child::*) = 1)]"))
    expect_that(xpath('e:only-of-type'),
                equals("e[(count(parent::*/child::e) = 1)]"))
    expect_that(xpath('e:empty'),
                equals("e[(not(*) and not(string-length()))]"))
    expect_that(xpath('e:EmPTY'),
                equals("e[(not(*) and not(string-length()))]"))
    expect_that(xpath('e:root'),
                equals("e[(not(parent::*))]"))
    expect_that(xpath('e:hover'),
                equals("e[(0)]")) #never matches
    expect_error(xpath('e:contains("foo")'),
                 "The pseudo-class :contains\\(\\) is unknown")
    expect_that(xpath('e.warning'),
                equals("e[(@class and contains(concat(' ', normalize-space(@class), ' '), ' warning '))]"))
    expect_that(xpath('e#myid'),
                equals("e[(@id = 'myid')]"))
    expect_that(xpath('e:not(:nth-child(odd))'),
                equals("e[(not((count(preceding-sibling::*) mod 2 = 0)))]"))
    expect_that(xpath('e:nOT(*)'),
                equals("e[(0)]")) # never matches
    # Selectors Level 4: :not() can nest inside functional pseudo-classes
    expect_that(xpath(':not(:not(a))'),
                equals("*[(not((not((name() = 'a')))))]"))
    expect_that(xpath('e:is(:not(f))'),
                equals("e[((not((name() = 'f'))))]"))
    expect_that(xpath('e:has(:not(f))'),
                equals("e[(.//*[(not((name() = 'f')))])]"))
    expect_that(xpath('e f'),
                equals("e//f"))
    expect_that(xpath('e > f'),
                equals("e/f"))
    expect_that(xpath('e + f'),
                equals("e/following-sibling::*[1][self::f]"))
    expect_that(xpath('e ~ f'),
                equals("e/following-sibling::f"))
    expect_that(xpath('e ~ f:nth-child(3)'),
                equals("e/following-sibling::f[(count(preceding-sibling::*) = 2)]"))
    expect_that(xpath('div#container p'),
                equals("div[(@id = 'container')]//p"))

    # expect that the following do nothing for the generic translator
    expect_that(xpath('a:any-link'), equals("a[(0)]"))
    expect_that(xpath('a:link'), equals("a[(0)]"))
    expect_that(xpath('a:visited'), equals("a[(0)]"))
    expect_that(xpath('a:hover'), equals("a[(0)]"))
    expect_that(xpath('a:active'), equals("a[(0)]"))
    expect_that(xpath('a:focus'), equals("a[(0)]"))
    expect_that(xpath('a:target'), equals("a[(0)]"))
    expect_that(xpath('a:target-within'), equals("a[(0)]"))
    expect_that(xpath('a:local-link'), equals("a[(0)]"))
    expect_that(xpath('a:enabled'), equals("a[(0)]"))
    expect_that(xpath('a:disabled'), equals("a[(0)]"))
    expect_that(xpath('a:checked'), equals("a[(0)]"))

    # Invalid characters in XPath element names

    charsets <- localeToCharset()
    if (!anyNA(charsets) && charsets[1] == "UTF-8") {
        expect_that(xpath('di\ua0v'),
                    equals("*[(name() = 'di v')]")) # div\ua0v
        expect_that(xpath('[h\ua0ref]'),
                    equals("*[(attribute::*[name() = 'h ref'])]")) # h\ua0ref
    }
    expect_that(xpath('di\\[v'),
                equals("*[(name() = 'di[v')]"))
    expect_that(xpath('[h\\]ref]'),
                equals("*[(attribute::*[name() = 'h]ref'])]"))
})
