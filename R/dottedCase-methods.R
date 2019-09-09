#' Dotted case
#'
#' @name dottedCase
#' @note [dottedCase()] support is provided for matching against base R
#'   parameters. However, it is recommended to avoid using it for variable
#'   assignments into an `environment`, as that can introduce conflicts with
#'   base functions.
#' @note Updated 2019-09-09.
#'
#' @inheritParams params
#'
#' @return Modified object, with names formatted in dotted case.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' lapply(syntactic, dottedCase)
NULL



## Dotted case formatting is used internally by other naming functions.
.dottedCase <-  # nolint
    function(object) {
        assert(is.atomic(object))
        object <- as.character(object)
        ## Handle "+" as a special case. Spell out as "plus".
        object <- gsub(
            pattern = "\\+",
            replacement = ".plus.",
            x = object
        )
        ## Handle "%" as a special case. Spell out as "percent".
        object <- gsub(
            pattern = "%",
            replacement = "percent",
            x = object
        )
        ## Strip comma delims in between numbers (e.g. 1,000,000).
        object <- gsub(
            pattern = "(\\d),(\\d)",
            replacement = "\\1\\2",
            x = object
        )
        ## Now we're ready to sanitize using base conventions.
        object <- make.names(
            names = object,
            unique = FALSE,
            allow_ = FALSE
        )
        ## Ensure all non-alphanumeric characters get coerced to periods.
        object <- gsub(
            pattern = "[^[:alnum:]]",
            replacement = ".",
            x = object
        )
        ## Combine multiple dots.
        object <- gsub(
            pattern = "[\\.]+",
            replacement = ".",
            x = object
        )
        ## Strip leading or trailing dots.
        object <- gsub(
            pattern = "(^\\.|\\.$)",
            replacement = "",
            x = object
        )
        ## Coerce `"NA"` back to `NA` after `make.names()` call.
        object <- gsub(
            pattern = "^NA$",
            replacement = NA_character_,
            x = object
        )
        ## Standardize any mixed case acronyms.
        object <- .sanitizeAcronyms(object)
        ## Establish word boundaries for camelCase acronyms
        ## (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`).
        ## Acronym following a word.
        object <- gsub(
            pattern = "([a-z])([A-Z])",
            replacement = "\\1.\\2",
            x = object
        )
        ## Word following an acronym.
        object <- gsub(
            pattern = "([A-Z0-9])([A-Z])([a-z])",
            replacement = "\\1.\\2\\3",
            x = object
        )
        ## Return.
        object
    }



`dottedCase,atomic` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("atomic"),
    definition = `dottedCase,atomic`
)



`dottedCase,character` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names <- .dottedCase(names(object))
        } else {
            names <- names(object)
        }
        object <- .dottedCase(object)
        names(object) <- names
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("character"),
    definition = `dottedCase,character`
)



`dottedCase,factor` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names <- dottedCase(names(object))
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- dottedCase(object)
        object <- as.factor(object)
        names(object) <- names
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("factor"),
    definition = `dottedCase,factor`
)



`dottedCase,list` <- `dottedCase,atomic`  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("list"),
    definition = `dottedCase,list`
)



`dottedCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("matrix"),
    definition = `dottedCase,matrix`
)



`dottedCase,data.frame` <- `dottedCase,matrix`  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("data.frame"),
    definition = `dottedCase,data.frame`
)



`dottedCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- dottedCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Vector"),
    definition = `dottedCase,Vector`
)



`dottedCase,DataTable` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- dottedCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object

    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("DataTable"),
    definition = `dottedCase,DataTable`
)



`dottedCase,Ranges` <- `dottedCase,Vector`  # nolint
formals(`dottedCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Ranges"),
    definition = `dottedCase,Ranges`
)



`dottedCase,Matrix` <- `dottedCase,matrix`  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Matrix"),
    definition = `dottedCase,Matrix`
)



`dottedCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <- dottedCase(names(assays(object)))
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <- dottedCase(colnames(rowData(object)))
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <- dottedCase(colnames(colData(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("SummarizedExperiment"),
    definition = `dottedCase,SummarizedExperiment`
)



#' @rdname dottedCase
#' @export
dotted <- function(...) {
    dottedCase(...)  # nocov
}
