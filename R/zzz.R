.onLoad <- function(libname, pkgname) { # nocov start
    reg_s3_method("XML", "querySelector", "XMLInternalNode", querySelector.XMLInternalNode)
    reg_s3_method("XML", "querySelectorAll", "XMLInternalNode", querySelectorAll.XMLInternalNode)
    reg_s3_method("XML", "querySelectorNS", "XMLInternalNode", querySelectorNS.XMLInternalNode)
    reg_s3_method("XML", "querySelectorAllNS", "XMLInternalNode", querySelectorAllNS.XMLInternalNode)

    reg_s3_method("XML", "querySelector", "XMLInternalDocument", querySelector.XMLInternalDocument)
    reg_s3_method("XML", "querySelectorAll", "XMLInternalDocument", querySelectorAll.XMLInternalDocument)
    reg_s3_method("XML", "querySelectorNS", "XMLInternalDocument", querySelectorNS.XMLInternalDocument)
    reg_s3_method("XML", "querySelectorAllNS", "XMLInternalDocument", querySelectorAllNS.XMLInternalDocument)

    reg_s3_method("xml2", "querySelector", "xml_node", querySelector.xml_node)
    reg_s3_method("xml2", "querySelectorAll", "xml_node", querySelectorAll.xml_node)
    reg_s3_method("xml2", "querySelectorNS", "xml_node", querySelectorNS.xml_node)
    reg_s3_method("xml2", "querySelectorAllNS", "xml_node", querySelectorAllNS.xml_node)

    invisible()
} # nocov end

reg_s3_method <- function(pkg, generic, class, fun = NULL) { # nocov start
    stopifnot(is.character(pkg), length(pkg) == 1)
    stopifnot(is.character(generic), length(generic) == 1)
    stopifnot(is.character(class), length(class) == 1)

    if (is.null(fun))
        fun <- get(paste0(generic, ".", class), envir = parent.frame())

    stopifnot(is.function(fun))

    if (pkg %in% loadedNamespaces()) {
        envir <- asNamespace("selectr")
        registerS3method(generic, class, fun, envir = envir)
    }

    # Register hook in case package is later unloaded & reloaded
    setHook(
        packageEvent(pkg, "onLoad"),
        function(...) {
            envir <- asNamespace("selectr")
            registerS3method(generic, class, fun, envir = envir)
        }
    )
} # nocov end
