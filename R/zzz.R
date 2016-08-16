.selectrEnv <- new.env()

getHasXML <- function() {
    get("hasXML", envir = .selectrEnv)
}

setHasXML <- function(hasXML) {
    assign("hasXML", hasXML, envir = .selectrEnv)
}

getHasxml2 <- function() {
    get("hasxml2", envir = .selectrEnv)
}

setHasxml2 <- function(hasxml2) {
    assign("hasxml2", hasxml2, envir = .selectrEnv)
}

hasXMLPackage <- function() {
    suppressPackageStartupMessages(requireNamespace("XML"))
}

hasxml2Package <- function() {
    suppressPackageStartupMessages(requireNamespace("xml2"))
}

registerXMLMethods <- function() {
    selectrNs <- getNamespace("selectr")

    registerS3method("querySelector", "XMLInternalNode", "querySelector.XMLInternalNode", envir = selectrNs)
    registerS3method("querySelectorAll", "XMLInternalNode", "querySelectorAll.XMLInternalNode", envir = selectrNs)
    registerS3method("querySelectorNS", "XMLInternalNode", "querySelectorNS.XMLInternalNode", envir = selectrNs)
    registerS3method("querySelectorAllNS", "XMLInternalNode", "querySelectorAllNS.XMLInternalNode", envir = selectrNs)

    registerS3method("querySelector", "XMLInternalDocument", "querySelector.XMLInternalDocument", envir = selectrNs)
    registerS3method("querySelectorAll", "XMLInternalDocument", "querySelectorAll.XMLInternalDocument", envir = selectrNs)
    registerS3method("querySelectorNS", "XMLInternalDocument", "querySelectorNS.XMLInternalDocument", envir = selectrNs)
    registerS3method("querySelectorAllNS", "XMLInternalDocument", "querySelectorAllNS.XMLInternalDocument", envir = selectrNs)
}

registerxml2Methods <- function() {
    selectrNs <- getNamespace("selectr")

    registerS3method("querySelector", "xml_node", "querySelector.xml_node", envir = selectrNs)
    registerS3method("querySelectorAll", "xml_node", "querySelectorAll.xml_node", envir = selectrNs)
    registerS3method("querySelectorNS", "xml_node", "querySelectorNS.xml_node", envir = selectrNs)
    registerS3method("querySelectorAllNS", "xml_node", "querySelectorAllNS.xml_node", envir = selectrNs)
}

tryLoadNamespaces <- function() {
    hasLoadedXML <- getHasXML()
    hasLoadedxml2 <- getHasxml2()

    if (hasLoadedXML && hasLoadedxml2)
        return()

    hasXML <- hasXMLPackage()
    hasxml2 <- hasxml2Package()

    if (!hasLoadedXML && hasXML)
        registerXMLMethods()

    if (!hasLoadedxml2 && hasxml2)
        registerxml2Methods()

    setHasXML(hasLoadedXML || hasXML)
    setHasxml2(hasLoadedxml2 || hasxml2)
}

.onLoad <- function(libname, pkgname) {
    # registering at runtime because we don't know if we will have objects
    # available to register methods for.
    # avoids hard dependency on XML or xml2
    hasXML <- hasXMLPackage()
    hasxml2 <- hasxml2Package()

    if (hasXMLPackage())
        registerXMLMethods()

    if (hasxml2Package())
        registerxml2Methods()

    setHasXML(hasXML)
    setHasxml2(hasxml2)
}
