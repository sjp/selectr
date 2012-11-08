\name{querySelectorAll}
\alias{querySelector}
\alias{querySelectorAll}
\title{
  Find nodes that match a group of CSS selectors in an XML tree.
}
\description{
  The purpose of these functions is to mimic the functionality of the
  \code{querySelector} and \code{querySelectorAll} functions present in
  Internet browsers. This is so we can succinctly query an XML tree for
  nodes matching a CSS selector.
}
\usage{
querySelector(doc, selector, ns = NULL, ...)
querySelectorAll(doc, selector, ns = NULL, ...)
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
    The namespaces that the query will be used with.
  }
  \item{...}{
    Parameters to be passed onto \code{css_to_xpath}.
  }
}
\details{
  The namespace argument, \code{ns}, is simply passed on to
  \code{\link[XML]{getNodeSet}} if it is necessary to match a namespace
  present within the document.
}
\value{
  For \code{querySelector}, the result is a single node that represents
  the first matched node from a selector. If no matching nodes are
  found, \code{NULL} is returned.

  For \code{querySelectorAll}, the result is a list of XML nodes. This
  list may be empty in the case that no match is found.
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
}
