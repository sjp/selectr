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
    The XML document or node to be evaluated against.
  }
  \item{selector}{
    A selector used to query \code{doc}. This must be a single character
    string.
  }
  \item{ns}{
    The namespace that the query will be filtered to. This is a named
    list or vector which has as its name a namespace, and its value is
    the namespace URI. This can be ignored for the un-namespaced
    functions.
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

  The namespace argument, \code{ns}, is simply passed on to
  \code{\link[XML]{getNodeSet}} if it is necessary to use a namespace
  present within the document. This can be ignored for content lacking a
  namespace, which is usually the case when using \code{querySelector}
  or \code{querySelectorAll}.
}
\value{
  For \code{querySelector}, the result is a single node that represents
  the first matched node from a selector. If no matching nodes are
  found, \code{NULL} is returned.

  For \code{querySelectorAll}, the result is a list of XML nodes. This
  list may be empty in the case that no match is found.

  The \code{querySelectorNS} and \code{querySelectorAllNS} functions
  return the same type of content as their un-namespaced counterparts.
}
\references{
  CSS3 Selectors \url{http://www.w3.org/TR/css3-selectors/}, XPath
  \url{http://www.w3.org/TR/xpath/}, querySelectorAll
  \url{https://developer.mozilla.org/en-US/docs/DOM/Document.querySelectorAll}
  and \url{http://www.w3.org/TR/selectors-api/#interface-definitions}.
}
\author{
  Simon Potter
}
\examples{
  library(XML)
  exdoc <- xmlParse(c('<a><b class="aclass"/><c id="anid"/></a>'))
  querySelector(exdoc, "#anid")   # Returns the matching node
  querySelector(exdoc, ".aclass") # Returns the matching node
  querySelector(exdoc, "b, c")    # First match from grouped selection
  querySelectorAll(exdoc, "b, c") # Grouped selection
  querySelectorAll(exdoc, "b")    # A list of length one
  querySelector(exdoc, "d")       # No match
  querySelectorAll(exdoc, "d")    # No match

  # Read in a document where two namespaces are being set:
  # SVG and MathML
  svgdoc <- xmlParse(system.file("demos/svg-mathml.svg",
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
