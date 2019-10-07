#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param names `logical(1)`.
#'   Sanitize names.
#' @param prefix `logical(1)`.
#'   Prepend "X" character if necessary, when string begins with a syntactically
#'   invalid character, such as a number or non-alphanumeric.
#'   See [`make.names()`][base::make.names] for details.
#' @param object Object.
#' @param strict `logical(1)`.
#'   Enforce strict name sanitization. When `TRUE`, this does not allow the
#'   return of any capitalized acronyms. "RNA" will become "Rna", for example.
#' @param string `character(1)`.
#' @param x Object.
#' @param ... Additional arguments.
NULL
