#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param names `logical(1)`.
#'   Sanitize names.
#'
#'   Only applies to string mode (`rename = FALSE`).
#' @param object Object.
#' @param prefix `logical(1)`.
#'   Prepend "X" character if necessary, when string begins with a syntactically
#'   invalid character, such as a number or non-alphanumeric. Note that names
#'   are always made syntactically valid when applicable with "X" prefix.
#'   See [`make.names()`][base::make.names] for details.
#'
#'   Enabled by default for string mode, but disabled by default for rename
#'   mode, when applicable.
#' @param recursive `logical(1)`.
#'   Should the function recurse into directories?
#'   Only applicable when `rename = TRUE`.
#' @param rename `logical(1)`.
#'   File rename mode. Requires that input contains files and/or directories
#'   that exist on disk. Both case-sensitive (e.g. Linux) and -insensitive (e.g.
#'   macOS, Winddows) file systems are supported.
#'
#'   The `prefix` and `smart` arguments are supported in this mode.
#' @param smart `logical(1)`.
#'   Handle complicated special cases, such as mixed case acronyms, plus/minus,
#'   percentages, etc.
#' @param strict `logical(1)`.
#'   Enforce strict name sanitization. When `TRUE`, this does not allow the
#'   return of any capitalized acronyms. "RNA" will become "Rna", for example.
#' @param unique `logical(1)`.
#'   If `TRUE`, the resulting elements are unique. Recommended by default, for
#'   syntactically valid names (e.g. column, row names). Note that this is
#'   disabled by default for [`make.names()`][base::make.names].
#' @param ... Additional arguments.
NULL
