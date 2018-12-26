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
#' @inheritParams params
#'
#' @param object `character` or class supporting `names`.
#' @param rownames `logical(1)`.
#'   Apply sanitization on row names. This is not generally recommended by
#'   default, since rownames commonly contain gene identifiers that should not
#'   be modified.
#' @param colnames `logical(1)`.
#'   Apply sanitization on column names. This is generally recommended by
#'   default.
#' @param strict `logical(1)`.
#'   Enforce strict name sanitization. When `TRUE`, this does not allow the
#'   return of any capitalized acronyms. "RNA" will become "Rna", for example.
#'
#' @return Modified object.
#' Contains syntatically valid names. For objects supporting `names`, the
#' underlying data returns unchanged.
#'
#' @seealso
#' - `make.names`.
#' - [janitor](https://cran.r-project.org/package=janitor) contains a number of
#'   useful functions that provide similar sanitization support, but isn't
#'   designed to work natively with [Bioconductor](https://bioconductor.org) and
#'   S4 object classes.
#' - [lettercase](https://cran.r-project.org/package=lettercase).
#' - [snakecase](https://tazinho.github.io/snakecase).
#'
#' @examples
#' loadRemoteData(url = file.path(basejumpCacheURL, "mn.rda"))
#'
#' ## character ====
#' x <- mn$character
#' print(x)
#' camel(x)
#' dotted(x)
#' snake(x)
#' upperCamel(x)
#' makeNames(x)
#'
#' x <- mn$namedCharacter
#' print(x)
#' camel(x)
#' dotted(x)
#' snake(x)
#' upperCamel(x)
#' makeNames(x)
#'
#' ## factor ====
#' x <- mn$factor
#' print(x)
#' camel(x)
#' dotted(x)
#' snake(x)
#' upperCamel(x)
#' makeNames(x)
#'
#' ## data.frame ====
#' x <- datasets::USArrests
#' dimnames(x)
#' camel(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' dotted(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' snake(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' upperCamel(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#'
#' ## list ====
#' x <- mn$list
#' print(x)
#' camel(x) %>% names()
#' dotted(x) %>% names()
#' snake(x) %>% names()
#' upperCamel(x) %>% names()
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
