# Fixture documents shared verbatim between the XML-package and
# xml2-package variants of a test (test-*-XML.R / test-*-xml2.R):
# testthat auto-sources every tests/testthat/helper-*.R file before
# running tests, so both variants can call these instead of each
# carrying its own copy of the literal.

# Used by test-select-XML.R and test-select-xml2.R
fixture_html_ids <- function() {
    paste0(
        c("<html id=\"html\"><head>", "  <link id=\"link-href\" href=\"foo\" />",
          "  <link id=\"link-nohref\" />", "</head><body>", "<div id=\"outer-div\">",
          " <a id=\"name-anchor\" name=\"foo\"></a>", " <a id=\"tag-anchor\" rel=\"tag\" href=\"http://localhost/foo\">link</a>",
          " <a id=\"nofollow-anchor\" rel=\"nofollow\" href=\"https://example.org\">",
          "    link</a>", " <ol id=\"first-ol\" class=\"a b c\">", "   <li id=\"first-li\">content</li>",
          "   <li id=\"second-li\" lang=\"En-us\">", "     <div id=\"li-div\">",
          "     </div>", "   </li>", "   <li id=\"third-li\" class=\"ab c\"></li>",
          "   <li id=\"fourth-li\" class=\"ab", "c\"></li>", "   <li id=\"fifth-li\"></li>",
          "   <li id=\"sixth-li\"></li>", "   <li id=\"seventh-li\">  </li>",
          " </ol>", " <p id=\"paragraph\">", "   <b id=\"p-b\">hi</b> <em id=\"p-em\">there</em>",
          "   <b id=\"p-b2\">guy</b>", "   <input type=\"checkbox\" id=\"checkbox-unchecked\" />",
          "   <input type=\"checkbox\" id=\"checkbox-disabled\" disabled=\"\" />",
          "   <input type=\"text\" id=\"text-checked\" checked=\"checked\" />",
          "   <input type=\"hidden\" />", "   <input type=\"hidden\" disabled=\"disabled\" />",
          "   <input type=\"checkbox\" id=\"checkbox-checked\" checked=\"checked\" />",
          "   <input type=\"checkbox\" id=\"checkbox-disabled-checked\"",
          "          disabled=\"disabled\" checked=\"checked\" />", "   <fieldset id=\"fieldset\" disabled=\"disabled\">",
          "     <input type=\"checkbox\" id=\"checkbox-fieldset-disabled\" />",
          "     <input type=\"hidden\" />", "   </fieldset>", " </p>",
          " <ol id=\"second-ol\">", " </ol>", " <map name=\"dummymap\">",
          "   <area shape=\"circle\" coords=\"200,250,25\" href=\"foo.html\" id=\"area-href\" />",
          "   <area shape=\"default\" id=\"area-nohref\" />", " </map>",
          "</div>", "<div id=\"foobar-div\" foobar=\"ab bc", "cde\"><span id=\"foobar-span\"></span></div>",
          "</body></html>"), collapse = "\n")
}

# Used by test-xmllang-XML.R and test-xmllang-xml2.R
fixture_xmllang <- function() {
    paste0('<test>',
          '<a id="first" xml:lang="en">a</a>',
          '<b id="second" xml:lang="en-US">b</b>',
          '<c id="third" xml:lang="en-Nz">c</c>',
          '<d id="fourth" xml:lang="En-us">d</d>',
          '<e id="fifth" xml:lang="fr">e</e>',
          '<f id="sixth" xml:lang="ru">f</f>',
          '<g id="seventh" xml:lang="de"><h id="eighth" xml:lang="zh" /></g>',
          '</test>')
}
