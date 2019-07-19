#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param colnames `logical(1)`.
#'   Apply sanitization on column names. This is generally recommended by
#'   default.
#' @param mcols `logical(1)`.
#'   FIXME
#' @param metadata `logical(1)`.
#'   FIXME
#' @param names `logical(1)`.
#'   Sanitize names.
#' @param object Object.
#' @param rownames `logical(1)`.
#'   Apply sanitization on row names. This is not generally recommended by
#'   default, since rownames commonly contain gene identifiers that should not
#'   be modified.
#' @param strict `logical(1)`.
#'   Enforce strict name sanitization. When `TRUE`, this does not allow the
#'   return of any capitalized acronyms. "RNA" will become "Rna", for example.
#' @param x Object.
NULL
