\name{querySelectorAll}
\alias{querySelector}
\alias{querySelectorAll}
\alias{querySelectorNS}
\alias{querySelectorAllNS}
\title{
  Find nodes that match a group of CSS selectors in an XML tree.
}
\description{
  The purpose of these functions is to mimic the functionality of the
  \code{querySelector} and \code{querySelectorAll} functions present in
  Internet browsers. This is so we can succinctly query an XML tree for
  nodes matching a CSS selector.

  Namespaced functions \code{querySelectorNS} and
  \code{querySelectorAllNS} are also provided to search relative to a
  given namespace.
}
\usage{
querySelector(doc, selector, ns = NULL, ...)
querySelectorAll(doc, selector, ns = NULL, ...)
querySelectorNS(doc, selector, ns,
                prefix = "descendant-or-self::", ...)
querySelectorAllNS(doc, selector, ns,
                   prefix = "descendant-or-self::", ...)
}
\arguments{
  \item{doc}{
    The XML document, node, or set of nodes to be evaluated against.
  }
  \item{selector}{
    A selector used to query \code{doc}. This must be a single character
    string.
  }
  \item{ns}{
    The namespaces that the query will be filtered to. This is a named
    list or vector whose name is the prefix a selector uses (the
    \code{svg} in \code{"svg|g"}), and whose value is the namespace URI
    that prefix stands for. Each name must be a valid XML name. This
    can be ignored for the un-namespaced functions, where a zero-length
    \code{ns} (\code{character(0)} or \code{list()}) additionally means
    no namespaces at all.
  }
  \item{prefix}{
    The prefix to apply to the resulting XPath expression. The default
    or \code{""} are most commonly used.
  }
  \item{...}{
    Parameters to be passed onto \code{css_to_xpath}.
  }
}
\details{
  The \code{querySelectorNS} and \code{querySelectorAllNS} functions are
  convenience functions for working with namespaced documents. They
  filter out all content that does not belong within the given
  namespaces. Note that when searching for particular elements in a
  selector, they must have a namespace prefix, e.g. \code{"svg|g"}.
  The filter is relative to \code{doc}, so like the un-namespaced
  functions these search a node's own subtree rather than the whole
  document. A selector starting with \code{:scope} replaces the filter
  altogether (see below); such a selector is namespaced by its own
  prefixes, e.g. \code{":scope > svg|g"}.

  The namespace argument, \code{ns}, is simply passed on to
  \code{\link[XML]{getNodeSet}} or \code{\link[xml2]{xml_find_all}} if
  it is necessary to use a namespace present within the document. This
  can be ignored for content lacking a namespace, which is usually the
  case when using \code{querySelector} or \code{querySelectorAll}.

  For \code{querySelector} and \code{querySelectorAll}, leaving
  \code{ns} as \code{NULL} on an \pkg{xml2} document means the
  document's own namespace map is used, which \code{\link[xml2]{xml_ns}}
  builds by walking the whole document on every query. Passing a
  zero-length \code{ns}, \code{character(0)} or \code{list()}, skips
  that lookup and queries with no namespaces, which is worth doing on a
  large document known to be un-namespaced. It is an error for the
  namespaced functions, which have nothing to filter to without a
  namespace.

  A selector's bare element names match elements in no namespace,
  \code{":is(p)"} and \code{":has(p)"} exactly as \code{"p"} itself
  does, so an element in a default namespace has to be reached through
  a prefix. With \pkg{xml2} the document's own prefixes are used when
  \code{ns} is not given, and \code{\link[xml2]{xml_ns}} names a
  default namespace \code{d1}, making \code{"d1|p"} the selector for
  those elements.

  Queries may be chained: as well as a document or a single node,
  \code{doc} may be a set of nodes, i.e. an \pkg{xml2}
  \code{xml_nodeset} or an \pkg{XML} \code{XMLNodeSet}, as returned
  by \code{querySelectorAll}. The selector is then evaluated from each
  node of the set in turn, so a relative selector such as \code{":scope
  > a"} applies per node. A node that matches from more than one node
  of the set is returned only once, at the position it first matched.
  An \pkg{xml2} \code{xml_missing} (the result of a failed
  \code{\link[xml2]{xml_find_first}}) is also accepted, and yields no
  matches rather than an error.

  Selectors are translated with the \code{generic} (XML) translator
  unless a \code{translator} argument is given to be passed on to
  \code{\link{css_to_xpath}}, with one exception: a
  document parsed as HTML by \code{\link[XML]{htmlParse}} or
  \code{\link[xml2]{read_html}} is queried with the \code{html}
  translator, so that element and attribute names are matched
  case-insensitively and the pseudo-classes that depend on HTML
  semantics (\code{:checked}, \code{:disabled}, \code{:link},
  \code{:lang()} via the \code{lang} attribute, ...) work as they do
  in a browser. Passing \code{translator} explicitly overrides this
  for either kind of document.

  The document is recognised however the query starts, so a chain of
  queries beginning at an HTML document keeps the \code{html}
  translator when it continues from one of the document's nodes or
  from a set of them.

  A selector starting with the \code{:scope} pseudo-class is anchored
  at the queried node itself: \code{querySelectorAll(node, ":scope >
  a")} returns only the \code{a} children of \code{node}, where
  \code{querySelectorAll(node, "a")} would return all of its \code{a}
  descendants. \code{:scope} after a combinator or within a functional
  pseudo-class is an error (it cannot be expressed in XPath 1.0).

  When \code{doc} is a whole document rather than a node, the queried
  node is taken to be the document's root element, so a bare
  \code{:scope} matches that root element and \code{":scope > x"}
  matches its \code{x} children. This differs from a browser's
  \code{document.querySelectorAll()}, where \code{:scope} on a
  document refers to the document itself: a bare \code{:scope} matches
  nothing there (the document is not an element), while \code{":scope
  > html"} matches the root element. To query starting from the root
  element itself rather than the document, pass the root node (e.g.
  \code{\link[XML]{xmlRoot}} or \code{\link[xml2]{xml_root}}) as
  \code{doc} instead of the document.
}
\value{
  For \code{querySelector}, the result is a single node that represents
  the first matched node from a selector. If no matching nodes are
  found, \code{NULL} is returned.

  For \code{querySelectorAll}, the result is a list of XML nodes. This
  list may be empty in the case that no match is found. The list is of
  the same type as the input document's package uses, so querying an
  \code{xml_nodeset} gives an \code{xml_nodeset} and querying an
  \code{XMLNodeSet} gives an \code{XMLNodeSet}.

  The \code{querySelectorNS} and \code{querySelectorAllNS} functions
  return the same type of content as their un-namespaced counterparts.
}
\section{Errors}{
  These functions propagate the same \code{selectr_parse_error} and
  \code{selectr_translation_error} conditions that
  \code{\link{css_to_xpath}} raises for a malformed or unsupported
  selector (see \code{?css_to_xpath} for their fields), plus
  \code{selectr_argument_error} for a bad R-level argument: \code{doc}
  that is not an \pkg{XML} or \pkg{xml2} document, node, or node set;
  \code{selector} that is not a single character string; or \code{ns}
  that is not a named list or named character vector of non-empty
  strings whose names are valid XML names (or, for
  \code{querySelectorNS} and \code{querySelectorAllNS}, a missing or
  zero-length \code{ns}).
}
\references{
  CSS Selectors Level 4 \url{https://www.w3.org/TR/selectors-4/}, XPath
  \url{https://www.w3.org/TR/xpath/}, querySelectorAll
  \url{https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelectorAll}
  and \url{https://dom.spec.whatwg.org/#dom-parentnode-queryselectorall}.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{css_to_xpath}}, whose \sQuote{Errors} section documents
  the condition classes' fields; \code{\link{selectors}} for the full
  selector-support reference.
}
\examples{
  # All three selectr_error classes (see 'Errors' below) propagate
  # from these functions; selectr_argument_error also covers a 'doc'
  # that is not an XML or xml2 document, node, or node set.
  tryCatch(
    querySelectorAll("not a document", "a"),
    selectr_argument_error = function(e) cat(conditionMessage(e), "\n")
  )

  # The XML and xml2 packages are both optional (Suggests), so each demo
  # below is guarded with requireNamespace() and runs only when that
  # package is installed.

  # Demo for working with the XML package
  if (requireNamespace("XML", quietly = TRUE)) {
    exdoc <- XML::xmlParse('<a><b class="aclass"/><c id="anid"/></a>')
    querySelector(exdoc, "#anid")   # Returns the matching node
    querySelector(exdoc, ".aclass") # Returns the matching node
    querySelector(exdoc, "b, c")    # First match from grouped selection
    querySelectorAll(exdoc, "b, c") # Grouped selection
    querySelectorAll(exdoc, "b")    # A list of length one
    querySelector(exdoc, "d")       # No match
    querySelectorAll(exdoc, "d")    # No match

    # Queries can be chained, the second search running from each node
    # matched by the first
    querySelectorAll(querySelectorAll(exdoc, "a"), "c")

    # Read in a document where two namespaces are being set:
    # SVG and MathML
    svgdoc <- XML::xmlParse(system.file("demos/svg-mathml.svg",
                                        package = "selectr"))
    # Search for <script/> elements in the SVG namespace
    querySelectorNS(svgdoc, "svg|script",
                    c(svg = "http://www.w3.org/2000/svg"))
    querySelectorAllNS(svgdoc, "svg|script",
                       c(svg = "http://www.w3.org/2000/svg"))
    # MathML content is *within* SVG content,
    # search for <mtext> elements within the MathML namespace
    querySelectorNS(svgdoc, "math|mtext",
                    c(math = "http://www.w3.org/1998/Math/MathML"))
    querySelectorAllNS(svgdoc, "math|mtext",
                       c(math = "http://www.w3.org/1998/Math/MathML"))
    # Search for *both* SVG and MathML content
    querySelectorAllNS(svgdoc, "svg|script, math|mo",
                       c(svg = "http://www.w3.org/2000/svg",
                         math = "http://www.w3.org/1998/Math/MathML"))
  }

  # Demo for working with the xml2 package
  if (requireNamespace("xml2", quietly = TRUE)) {
    exdoc <- xml2::read_xml('<a><b class="aclass"/><c id="anid"/></a>')
    querySelector(exdoc, "#anid")   # Returns the matching node
    querySelector(exdoc, ".aclass") # Returns the matching node
    querySelector(exdoc, "b, c")    # First match from grouped selection
    querySelectorAll(exdoc, "b, c") # Grouped selection
    querySelectorAll(exdoc, "b")    # A nodeset of length one
    querySelector(exdoc, "d")       # No match
    querySelectorAll(exdoc, "d")    # No match

    # Queries can be chained, the second search running from each node
    # matched by the first
    querySelectorAll(querySelectorAll(exdoc, "a"), "c")

    # Read in a document where two namespaces are being set:
    # SVG and MathML
    svgdoc <- xml2::read_xml(system.file("demos/svg-mathml.svg",
                                         package = "selectr"))
    # Search for <script/> elements in the SVG namespace
    querySelectorNS(svgdoc, "svg|script",
                    c(svg = "http://www.w3.org/2000/svg"))
    querySelectorAllNS(svgdoc, "svg|script",
                       c(svg = "http://www.w3.org/2000/svg"))
    # MathML content is *within* SVG content,
    # search for <mtext> elements within the MathML namespace
    querySelectorNS(svgdoc, "math|mtext",
                    c(math = "http://www.w3.org/1998/Math/MathML"))
    querySelectorAllNS(svgdoc, "math|mtext",
                       c(math = "http://www.w3.org/1998/Math/MathML"))
    # Search for *both* SVG and MathML content
    querySelectorAllNS(svgdoc, "svg|script, math|mo",
                       c(svg = "http://www.w3.org/2000/svg",
                         math = "http://www.w3.org/1998/Math/MathML"))
  }
}
