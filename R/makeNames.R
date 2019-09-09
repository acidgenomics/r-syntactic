#' Make syntactically valid names
#'
#' For `atomic` vectors, these functions will sanitize the values. Otherwise,
#' they will set [`names()`][base::names], [`rownames()`][base::rownames()],
#' and/or [`colnames()`][base::colnames] without modification of the values.
#'
#' @export
#' @note [makeNames()] sanitizes names using underscores instead of dots, the
#' convention used by [`make.names()`][base::make.names].
#' @note Updated 2019-09-09.
#'
#' @param names `character`.
#'   Character vector to be coerced to syntactically valid names.
#' @param unique `logical(1)`.
#'   If `TRUE`, the resulting elements are unique. Recommended by default, for
#'   syntactically valid names (e.g. column, row names). Note that this is
#'   disabled by default for [`make.names()`][base::make.names].
#'
#' @seealso [`make.names()`][base::make.names].
#'
#' @return `character`.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' names <- syntactic$character
#' makeNames(names)
makeNames <- function(names, unique = TRUE) {
    assert(
        is.atomic(names),
        isFlag(unique)
    )
    x <- as.character(names)
    x <- make.names(names = x, unique = unique)
    x <- gsub(pattern = "\\.", replacement = "_", x = x)
    x
}
