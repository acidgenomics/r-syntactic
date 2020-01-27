## nocov start
## nolint start



#' @name deprecated
#' @inherit acidroxygen::deprecated description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



## v0.3.6 ======================================================================
#' @rdname deprecated
#' @export
camel <- function(...) {
    .Deprecated("camelCase")
    camelCase(...)
}



## nolint end
## nocov end
