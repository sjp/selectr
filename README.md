# selectr

[![License (3-Clause BSD)](https://img.shields.io/badge/license-BSD%203--Clause-blue.svg)](https://opensource.org/license/BSD-3-Clause) [![GitHub Actions](https://github.com/sjp/selectr/actions/workflows/r.yml/badge.svg)](https://github.com/sjp/selectr/actions/workflows/r.yml) [![CRAN version](https://www.r-pkg.org/badges/version/selectr)](https://cran.r-project.org/package=selectr) [![codecov](https://codecov.io/gh/sjp/selectr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/sjp/selectr) ![Downloads per month](https://cranlogs.r-pkg.org/badges/last-month/selectr)

selectr is a package which makes working with HTML and XML documents easier. It does this by performing translation of CSS selectors into XPath expressions so that you can query `XML` and `xml2` documents easily.

``` r
library(selectr)
xpath <- css_to_xpath("#selectr")
xpath
#> [1] "descendant-or-self::*[@id = 'selectr']"
```

## Installation

### Install the release version from CRAN

``` r
install.packages("selectr")
```

### Install the development version from GitHub

``` r
# install.packages("remotes")
remotes::install_github("sjp/selectr")
```

## Overview

The key functions in selectr are:

* Translate a CSS selector into an XPath expression with `css_to_xpath()`.

* Query an `XML` or `xml2` document with `querySelector()` and its variants.

    * Find the first matching node with `querySelector()`.

    * Find all matching nodes with `querySelectorAll()`.

    * Find the first matching node in a namespaced document with `querySelectorNS()`.

    * Find all matching nodes in a namespaced document with `querySelectorAllNS()`.

Documents read with `htmlParse()` (`XML`) or `read_html()` (`xml2`) are auto-detected and queried with the HTML translator, so `:checked`, `:disabled`, `:link` and case-insensitive names all work without passing `translator = "html"` yourself. Queries also chain: `querySelectorAll()` accepts a node set as well as a document, so `querySelectorAll(querySelectorAll(doc, "table"), "tr")` runs the second selector from each node the first matched. `:scope`, `:is()`, `:where()`, `:has()` and `:nth-child(An+B of S)` are all supported. See `?selectors` for the full table of what selectr supports and what each translates to, and `?css_to_xpath` for the reasoning behind its more surprising entries.

## Examples

### Scraping an HTML document

``` r
library(selectr)
library(xml2)

html <- paste0(
  "<html><body>",
  "<table id='products'>",
  "<tr><td class='name'>Widget</td><td class='price'>9.99</td></tr>",
  "<tr><td class='name'>Gadget</td><td class='price'>19.99</td></tr>",
  "</table>",
  "<input type='checkbox' checked>",
  "</body></html>")
doc <- read_html(html)

# :has() picks out the table, chaining then walks its rows
rows <- querySelectorAll(doc, "table:has(.price) tr")
querySelectorAll(rows, "td.name")
#> {xml_nodeset (2)}
#> [1] <td class="name">Widget</td>
#> [2] <td class="name">Gadget</td>

# The html translator (auto-detected from read_html()) implements :checked
querySelector(doc, "input:checked")
#> {html_node}
#> <input type="checkbox" checked="checked">
```

### Querying a namespaced XML document

``` r
library(selectr)
library(xml2)

# A document with both SVG and MathML content
svgdoc <- read_xml(system.file("demos/svg-mathml.svg", package = "selectr"))
querySelectorAllNS(svgdoc, "svg|script, math|mo",
                   c(svg = "http://www.w3.org/2000/svg",
                     math = "http://www.w3.org/1998/Math/MathML"))
```

Parsing a large namespaced document with `xml2` builds its namespace map on every call by default; pass `ns = character(0)` to `querySelector()`/`querySelectorAll()` to skip that lookup when the document is known to be un-namespaced.

### Structured, position-annotated errors

Every error `css_to_xpath()` and the `querySelector*()` functions raise inherits `selectr_error`, so a caller can catch the whole family or a specific class such as `selectr_parse_error`, which also carries the 1-based character position the parser gave up at:

``` r
tryCatch(
  css_to_xpath("div >"),
  selectr_parse_error = function(e) cat(conditionMessage(e), "\n")
)
#> Expected selector, got <EOF at 6>
#>   |
#>   | div >
#>   |      ^
```

See `?css_to_xpath` (section "Errors") for the full condition hierarchy.

## Development

* Run the test suite: `R -e 'pkgload::load_all(); testthat::test_dir("tests/testthat")'`.
* Lint the package: `R -e 'lintr::lint_package()'` (configured by `.lintr`).
* Build and run a full CRAN-style check with the Makefile: `make check`
  (or `make build` to only build the source tarball).

A [devcontainer](.devcontainer) is provided with R and the package's dependencies preinstalled.
