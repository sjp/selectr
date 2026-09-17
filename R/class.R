# A minimal stand-in for the part of R6 the translators use, so that
# the package has no dependencies beyond base R. translator_class()
# returns a generator with a $new() method and the class's own
# $public_methods (inherited ones excluded, as with R6). An instance is
# a locked environment holding the fields and methods of the class and
# of its ancestors, a subclass's definitions replacing those it
# inherits. Every method is a copy whose enclosing environment binds
# 'self' to the instance (with the environment the class was defined
# in as its parent), so methods call each other, including a
# subclass's overrides, through self$name() or self[[name]].
translator_class <- function(classname, public = list(), inherit = NULL,
                             parent_env = parent.frame()) {
    is_method <- vapply(public, is.function, logical(1))
    generator <- new.env(parent = emptyenv())
    generator$classname <- c(classname, inherit$classname)
    generator$public_fields <- public[!is_method]
    generator$public_methods <- public[is_method]
    generator$parent_env <- parent_env
    generator$inherit <- inherit

    generator$new <- function(...) {
        self <- new.env(parent = emptyenv())
        # Walk from the base class down, so a subclass's definitions
        # overwrite what it inherits
        chain <- list()
        gen <- generator
        while (!is.null(gen)) {
            chain <- c(list(gen), chain)
            gen <- gen$inherit
        }
        for (gen in chain) {
            list2env(gen$public_fields, envir = self)
            enclos <- new.env(parent = gen$parent_env)
            enclos$self <- self
            for (name in names(gen$public_methods)) {
                method <- gen$public_methods[[name]]
                environment(method) <- enclos
                assign(name, method, envir = self)
            }
        }
        class(self) <- generator$classname
        if (is.function(self$initialize))
            self$initialize(...)
        else if (...length())
            stop("Called new() with arguments, but there is no initialize method.",
                 call. = FALSE)
        lockEnvironment(self)
        for (name in ls(self, all.names = TRUE))
            if (is.function(self[[name]]))
                lockBinding(name, self)
        self
    }
    generator
}
