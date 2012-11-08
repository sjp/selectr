\name{css_to_xpath}
\alias{css_to_xpath}
\title{
  Translate a CSS selector to an equivalent XPath expression.
}
\description{
  This function aims to create an XPath expression equivalent to what
  would be matched by the given CSS selector. The reason the translation
  is required is because the XML package, being a libxml2 wrapper, can
  only evaluate XPath expressions.

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
  CSS3 Selectors \url{http://www.w3.org/TR/css3-selectors/}, XPath
  \url{http://www.w3.org/TR/xpath/}.
}
\author{
  Simon Potter
}
\examples{
  css_to_xpath(".testclass")
  css_to_xpath("#testid", prefix = "")
  css_to_xpath("#testid .testclass")
  css_to_xpath(":checked", translator = "html")
}
