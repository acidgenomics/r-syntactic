#' Make syntactically valid names
#'
#' For `atomic` vectors, these functions will sanitize the values. Otherwise,
#' they will set `names`, `rownames`, and/or `colnames` without
#' modification of the values.
#'
#' @note `makeNames` sanitizes names using underscores instead of dots, the
#' convention used by `make.names`.
#'
#' @export
#' @inheritParams base::make.names
#'
#' @return Modified object.
#' Contains syntatically valid names. For objects supporting `names`, the
#' underlying data returns unchanged.
#'
#' @seealso `make.names()`.
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
