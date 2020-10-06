## nocov start
## nolint start



#' @name deprecated
#' @inherit AcidRoxygen::deprecated description examples return seealso title
#' @inheritParams AcidRoxygen::params
#' @keywords internal
NULL



## v0.3.6 ======================================================================
#' @rdname deprecated
#' @export
camel <- function(...) {
    .Deprecated("camelCase")
    camelCase(...)
}

#' @rdname deprecated
#' @export
dotted <- function(...) {
    .Deprecated("dottedCase")
    dottedCase(...)
}

#' @rdname deprecated
#' @export
kebab <- function(...) {
    .Deprecated("kebabCase")
    kebabCase(...)
}

#' @rdname deprecated
#' @export
snake <- function(...) {
    .Deprecated("snakeCase")
    snakeCase(...)
}

#' @rdname deprecated
#' @export
upperCamel <- function(...) {
    .Deprecated("upperCamelCase")
    upperCamelCase(...)
}



## nolint end
## nocov end
