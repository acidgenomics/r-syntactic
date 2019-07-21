#' @name dottedCase
#'
#' @note [dottedCase()] support is provided for matching against base R
#'   parameters. However, it is recommended to avoid using it for variable
#'   assignments into an `environment`, as that can introduce conflicts with
#'   base functions.
#'
#' @inherit bioverbs::dottedCase
#' @inherit camel return
#' @inheritParams params
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' lapply(syntactic, dottedCase)
NULL



#' @rdname dottedCase
#' @name dottedCase
#' @importFrom bioverbs dottedCase
#' @usage dottedCase(object, ...)
#' @export
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

        object
    }



## Base R classes ==============================================================
## Updated 2019-07-21.
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



## Updated 2019-07-21.
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



## Updated 2019-07-21.
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



## Updated 2019-07-21.
`dottedCase,list` <- `dottedCase,atomic`  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("list"),
    definition = `dottedCase,list`
)



## Updated 2019-07-21.
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



## Updated 2019-07-21.
`dottedCase,data.frame` <- `dottedCase,matrix`  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("data.frame"),
    definition = `dottedCase,data.frame`
)



## S4 virtual classes ==========================================================
## Updated 2019-07-19.
`dottedCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        validObject(object)
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            mcolnames(object) <- dottedCase(mcolnames(object))
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



## Updated 2019-07-19.
## mcols metadata
`dottedCase,DataTable` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        validObject(object)
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
            mcolnames(object) <- dottedCase(mcolnames(object))
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



## Updated 2019-07-19.
`dottedCase,Ranges` <- `dottedCase,Vector`  # nolint
formals(`dottedCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Ranges"),
    definition = `dottedCase,Ranges`
)



## Updated 2019-07-19.
`dottedCase,Matrix` <- `dottedCase,matrix`  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Matrix"),
    definition = `dottedCase,Matrix`
)



## S4 classes ==================================================================
## Updated 2019-07-19.
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
        validObject(object)
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



## Aliases =====================================================================
#' @rdname dottedCase
#' @export
dotted <- function(...) {
    dottedCase(...)  # nocov
}
