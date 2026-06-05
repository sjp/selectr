\name{css_to_xpath}
\alias{css_to_xpath}
\title{
  Translate a CSS selector to an equivalent XPath expression.
}
\description{
  This function aims to create an XPath expression equivalent to what
  would be matched by the given CSS selector. The reason the translation
  is required is because the XML and xml2 packages, being a libxml2
  wrappers, can only evaluate XPath expressions.

  Using this function, it is possible to search an XML tree without the
  prerequisite of knowing XPath.
}
\usage{
css_to_xpath(selector,
             prefix = "descendant-or-self::",
             translator = "generic")
}
\arguments{
  \item{selector}{
    A character vector of CSS selectors.
  }
  \item{prefix}{
    The prefixes to apply to the resulting XPath expressions. The
    default or \code{""} are most commonly used.
  }
  \item{translator}{
    The type of translator that will be used. Possible options are
    \code{generic} (the default), or \code{html} or \code{xhtml}.
  }
}
\details{
  Each selector given to this function will be translated to an
  equivalent XPath expression. The resulting XPath expression can be
  given a prefix which determines the scope of the expression. The
  default prefix determines the scope to be the node itself and all
  descendants of the node. Most commonly the prefix is either the
  default or \code{""}, unless it is known what scope a particular XPath
  expression should have.

  A selector starting with the \code{:scope} pseudo-class is anchored
  at the node the expression is evaluated from: the \code{prefix}
  argument is ignored and the expression begins with the XPath
  \code{self} axis instead. For example, \code{":scope > a"} translates
  to \code{"self::*/a"}, matching only the \code{a} children of the
  queried node, and a bare \code{":scope"} translates to
  \code{"self::*"}, matching the queried node itself. \code{:scope}
  anywhere else in a selector (after a combinator, or within a
  functional pseudo-class such as \code{:is()} or \code{:has()}) cannot
  be expressed in XPath 1.0 and is an error.

  The translator used is usually unnecessary to specify as the default
  is sufficient for most cases. However, it is of use when creating
  expressions relating to (X)HTML pseudo elements and languages. In
  particular it qualifies the following pseudo selectors to apply only
  to relevant (X)HTML elements: \code{:checked}, \code{:disabled},
  \code{:enabled} and \code{:link}.

  When the translator is set to \code{html}, all elements and
  attributes will be converted to lower case. This restriction is
  removed when the translator is \code{xhtml} (or the default
  \code{generic} translator).
}
\value{
  A character vector of XPath expressions.
}
\references{
  CSS Selectors Level 4 \url{https://www.w3.org/TR/selectors-4/}, XPath
  \url{https://www.w3.org/TR/xpath/}.
}
\author{
  Simon Potter
}
\examples{
  css_to_xpath(".testclass")
  css_to_xpath("#testid", prefix = "")
  css_to_xpath("#testid .testclass")
  css_to_xpath(":scope > .testclass")
  css_to_xpath(":checked", translator = "html")
}
