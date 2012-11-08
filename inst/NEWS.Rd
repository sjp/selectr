\name{NEWS}
\title{NEWS file for the selectr package}
\encoding{UTF-8}

\section{Changes in version 0.1-1}{
  \subsection{BUG FIXES}{
    \itemize{

      \item Now testing unicode only in non-Windows platforms on package
      check. Output should still be consistent, just depends on the
      current charset being unicode.

    }
  }
}

\section{Changes in version 0.1-0}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Initial port of the Python 'cssselect' package. Code is very
      literally ported, including the test suite.

      \item Wrapped translation functionality into a single function,
      'css_to_xpath()'.

      \item Created two convenience functions, 'querySelector()' and
      'querySelectorAll()'. These mirror the behaviour of the same
      functions present in a web browser. 'querySelector()' returns a
      node, while 'querySelectorAll()' returns a list of nodes.

    }
  }
}
