#' Make syntactically valid names
#'
#' For `atomic` vectors, these functions will sanitize the values. Otherwise,
#' they will set [`names()`][base::names], [`rownames()`][base::rownames()],
#' and/or [`colnames()`][base::colnames] without modification of the values.
#'
#' @name makeNames
#' @note Sanitizes names using underscores instead of dots, the convention used
#'   by [`make.names()`][base::make.names].
#' @note Updated 2019-10-21.
#'
#' @inheritParams params
#' @param unique `logical(1)`.
#'   If `TRUE`, the resulting elements are unique. Recommended by default, for
#'   syntactically valid names (e.g. column, row names). Note that this is
#'   disabled by default for [`make.names()`][base::make.names].
#'
#' @seealso [`make.names()`][base::make.names].
#'
#' @return Modified object.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' makeNames(object)
NULL



`makeNames,character` <-  # nolint
    function(object, unique = TRUE) {
    assert(
        isCharacter(object),
        isFlag(unique)
    )
    x <- make.names(names = object, unique = unique)
    x <- gsub(pattern = "\\.", replacement = "_", x = x)
    x
}



#' @rdname makeNames
#' @export
setMethod(
    f = "makeNames",
    signature = signature("character"),
    definition = `makeNames,character`
)
