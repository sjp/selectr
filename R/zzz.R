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
    suppressPackageStartupMessages(requireNamespace("XML", quietly = TRUE))
}

hasxml2Package <- function() {
    suppressPackageStartupMessages(requireNamespace("xml2", quietly = TRUE))
}

registerXMLMethods <- function() {
    registerMethods(c("XMLInternalNode", "XMLInternalDocument"))
}

registerxml2Methods <- function() {
    registerMethods("xml_node")
}

registerMethods <- function(objectNames) {
    selectrNs <- getNamespace("selectr")
    for (methodName in .qsMethodNames) {
        for (objectName in objectNames) {
            methodImplName <- paste(methodName, objectName, sep = ".")
            registerS3method(methodName, objectName, methodImplName, envir = selectrNs)
        }
    }
}

.qsMethodNames <- c("querySelector", "querySelectorAll",
                    "querySelectorNS", "querySelectorAllNS")

# registering at runtime because we don't know if we will have objects
# available to register methods for.
# avoids hard dependency on XML or xml2
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

# Not covered as it is only run during a package load.
# Tested via tryLoadNamespaces anyway.
.onLoad <- function(libname, pkgname) { # nocov start
    setHasXML(FALSE)
    setHasxml2(FALSE)
    tryLoadNamespaces()
} # nocov end
