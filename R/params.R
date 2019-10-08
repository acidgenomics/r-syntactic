#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param names `logical(1)`.
#'   Sanitize names.
#' @param object Object.
#' @param prefix `logical(1)`.
#'   Prepend "X" character if necessary, when string begins with a syntactically
#'   invalid character, such as a number or non-alphanumeric. Note that names
#'   are always made syntactically valid when applicable with "X" prefix.
#'   See [`make.names()`][base::make.names] for details. Recommended by default.
#' @param smart `logical(1)`.
#'   Handle complicated special cases, such as mixed case acronyms, plus/minus,
#'   percentages, etc. Recommended by default.
#' @param strict `logical(1)`.
#'   Enforce strict name sanitization. When `TRUE`, this does not allow the
#'   return of any capitalized acronyms. "RNA" will become "Rna", for example.
#'   Disabled by default.
#' @param string `character(1)`.
#' @param x Object.
#' @param ... Additional arguments.
NULL
