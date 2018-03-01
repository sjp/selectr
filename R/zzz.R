.onLoad <- function(libname, pkgname) { # nocov start
    regS3Method("XML", "querySelector", "XMLInternalNode")
    regS3Method("XML", "querySelectorAll", "XMLInternalNode")
    regS3Method("XML", "querySelectorNS", "XMLInternalNode")
    regS3Method("XML", "querySelectorAllNS", "XMLInternalNode")

    regS3Method("XML", "querySelector", "XMLInternalDocument")
    regS3Method("XML", "querySelectorAll", "XMLInternalDocument")
    regS3Method("XML", "querySelectorNS", "XMLInternalDocument")
    regS3Method("XML", "querySelectorAllNS", "XMLInternalDocument")

    regS3Method("xml2", "querySelector", "xml_node")
    regS3Method("xml2", "querySelectorAll", "xml_node")
    regS3Method("xml2", "querySelectorNS", "xml_node")
    regS3Method("xml2", "querySelectorAllNS", "xml_node")

    invisible()
} # nocov end

regS3Method <- function(pkg, generic, class, fun = NULL) { # nocov start
    stopifnot(is.character(pkg), length(pkg) == 1)
    stopifnot(is.character(generic), length(generic) == 1)
    stopifnot(is.character(class), length(class) == 1)
    
    if (is.null(fun))
        fun <- get(paste0(generic, ".", class), envir = parent.frame())

    stopifnot(is.function(fun))

    if (pkg %in% loadedNamespaces()) {
        envir <- asNamespace(pkg)
        registerS3method(generic, class, fun, envir = envir)
    }

    # Register hook in case package is later unloaded & reloaded
    setHook(
        packageEvent(pkg, "onLoad"),
        function(...) {
            envir <- asNamespace(pkg)
            registerS3method(generic, class, fun, envir = envir)
        }
    )
} # nocov end
