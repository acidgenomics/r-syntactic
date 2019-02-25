#' @name kebab
#'
#' @note We're not including the additional S4 methods that work on
#' [`names()`][base::names] and/or [`dimnames()`][base::dimnames] because dashes
#' are not syntactically valid for names in R.
#'
#' @inherit bioverbs::kebab
#' @inheritParams params
#'
#' @return Modified object.
#'
#' @examples
#' kebab("this is kebab case")
NULL



#' @importFrom bioverbs kebab
#' @aliases NULL
#' @export
bioverbs::kebab



# Note that by calling `snake()` internally, this will handle special words
# like "%" and "+", which we want. Refer to `dotted()` for this code.
kebab.character <- function(object) {
    names <- names(object)
    object <- snake(object)
    object <- gsub(pattern = "_", replacement = "-", x = object)
    names(object) <- names
    object
}



#' @rdname kebab
#' @export
setMethod(
    f = "kebab",
    signature = signature("character"),
    definition = kebab.character
)
