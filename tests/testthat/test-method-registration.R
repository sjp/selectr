context("methods")

test_that("environment variables get and set correctly", {
    # must be true for us to be testing
    expect_that(hasXMLPackage(), equals(TRUE))
    expect_that(hasxml2Package(), equals(TRUE))

    # init variables (we'll be changing them later, want a baseline)
    library(XML)
    xdoc <- xmlParse("<svg><circle /></svg>")

    library(xml2)
    x2doc <- read_xml("<svg><circle /></svg>")

    results <- querySelector(xdoc, "circle")
    results <- querySelector(x2doc, "circle")

    # should be true seeing as we should have namespaces loaded now
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    # pretend that the packages haven't been loaded yet
    setHasXML(FALSE)
    setHasxml2(FALSE)
    expect_that(getHasXML(), equals(FALSE))
    expect_that(getHasxml2(), equals(FALSE))

    # now let's assign back
    setHasXML(TRUE)
    setHasxml2(TRUE)
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))
})

test_that("method registration occurs correctly", {
    # pretend that the packages haven't been loaded yet
    setHasXML(FALSE)
    setHasxml2(FALSE)

    tryLoadNamespaces()

    # should now have env vars set
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    tryLoadNamespaces()

    # expect no change to the env vars after running a second time
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))
})

test_that("method registration occurs when querySelector methods are called", {
    # pretend that the packages haven't been loaded yet
    unsetPackages <- function() {
        setHasXML(FALSE)
        setHasxml2(FALSE)
    }
    unsetPackages()

    library(XML)
    xdoc <- xmlParse("<svg><circle /></svg>")

    library(xml2)
    x2doc <- read_xml("<svg><circle /></svg>")

    # querySelector
    result <- querySelector(xdoc, "circle")
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    unsetPackages()

    result <- querySelector(x2doc, "circle")
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    unsetPackages()

    # querySelectorAll
    result <- querySelectorAll(xdoc, "circle")
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    unsetPackages()

    result <- querySelectorAll(x2doc, "circle")
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    unsetPackages()


    xdoc <- xmlParse('<svg xmlns="http://www.w3.org/2000/svg"><circle /></svg>')
    x2doc <- read_xml('<svg xmlns="http://www.w3.org/2000/svg"><circle /></svg>')

    # querySelectorNS
    result <- querySelectorNS(xdoc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    unsetPackages()

    result <- querySelectorNS(x2doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    # querySelectorAllNS
    result <- querySelectorAllNS(xdoc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    unsetPackages()

    result <- querySelectorAllNS(x2doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))
    expect_that(getHasXML(), equals(TRUE))
    expect_that(getHasxml2(), equals(TRUE))

    # finally check that methods() can see them
    qsMethods <- methods("querySelector")
    qsAllMethods <- methods("querySelectorAll")
    qsNSMethods <- methods("querySelectorNS")
    qsAllNSMethods <- methods("querySelectorAllNS")

    objectNames <- c("xml_node", "XMLInternalDocument", "XMLInternalNode")

    qsMethodNames <- paste("querySelector", objectNames, sep = ".")
    qsAllMethodNames <- paste("querySelectorAll", objectNames, sep = ".")
    qsNSMethodNames <- paste("querySelectorNS", objectNames, sep = ".")
    qsAllNSMethodNames <- paste("querySelectorAllNS", objectNames, sep = ".")

    expect_that(all(qsMethodNames %in% qsMethods), equals(TRUE))
    expect_that(all(qsAllMethodNames %in% qsAllMethods), equals(TRUE))
    expect_that(all(qsNSMethodNames %in% qsNSMethods), equals(TRUE))
    expect_that(all(qsAllNSMethodNames %in% qsAllNSMethods), equals(TRUE))
})
