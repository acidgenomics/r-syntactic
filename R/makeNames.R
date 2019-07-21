#' Make syntactically valid names
#'
#' For `atomic` vectors, these functions will sanitize the values. Otherwise,
#' they will set [`names()`][base::names], [`rownames()`][base::rownames()],
#' and/or [`colnames()`][base::colnames] without modification of the values.
#'
#' @note [makeNames()] sanitizes names using underscores instead of dots, the
#' convention used by [`make.names()`][base::make.names].
#'
#' @export
#' @inheritParams base::make.names
#'
#' @seealso `make.names()`.
#'
#' @return `character`.
#'
#' @examples
#' data(mn, package = "acidtest")
#' names <- mn$character
#' makeNames(names)

## Updated 2019-07-19.
makeNames <- function(names, unique = TRUE) {
    assert(
        is.atomic(names),
        isFlag(unique)
    )
    names <- as.character(names)
    names <- make.names(names, unique = unique)
    names <- gsub("\\.", "_", names)
    names
}
