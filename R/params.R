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
#' @param rename `logical(1)`.
#'   File rename mode. Requires that input contains files and/or directories
#'   that exist on disk. Either case-sensitive (e.g. Linux) and -insensitive
#'   (e.g. macOS, Winddows) file systems are supported. The `prefix` and `smart`
#'   arguments are supported in this mode.
#' @param smart `logical(1)`.
#'   Handle complicated special cases, such as mixed case acronyms, plus/minus,
#'   percentages, etc. Recommended by default.
#' @param strict `logical(1)`.
#'   Enforce strict name sanitization. When `TRUE`, this does not allow the
#'   return of any capitalized acronyms. "RNA" will become "Rna", for example.
#'   Disabled by default.
#' @param ... Additional arguments.
NULL
