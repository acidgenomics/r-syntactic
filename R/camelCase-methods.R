#' @name camelCase
#'
#' @inherit bioverbs::camelCase
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Contains syntatically valid names. For objects supporting
#' [`names()`][base::names], the underlying data returns unchanged, except for
#' `character` or `vector` class.
#'
#' @examples
#' data(mn, package = "acidtest")
#' lapply(mn, camelCase)
NULL



#' @rdname camelCase
#' @name camelCase
#' @importFrom bioverbs camelCase
#' @usage camelCase(object, ...)
#' @export
NULL



## Updated 2019-07-19.
.camelCase <-  # nolint
    function(
        object,
        format = c("lower", "upper"),
        strict = FALSE
    ) {
        object <- dotted(object)
        format <- match.arg(format)
        assert(isFlag(strict))

        ## Simplify mixed case acronyms in strict mode.
        if (isTRUE(strict)) {
            object <- tolower(object)
        }

        ## lowerCamelCase or UpperCamelCase.
        if (format == "lower") {
            ## lowerCamelCase
            ## Coerce first word to lower.
            object <- gsub(
                pattern = "^(\\w+)\\b",
                replacement = "\\L\\1",
                x = object,
                perl = TRUE
            )
        } else if (format == "upper") {
            ## UpperCamelCase
            ## Capitalize the first letter.
            object <- gsub(
                pattern = "^([a-z])",
                replacement = "\\U\\1",
                x = object,
                perl = TRUE
            )
        }

        ## Remove dots in between numbers following a letter.
        object <- gsub("([[:alpha:]])\\.([[:digit:]])", "\\1\\2", object)

        ## First letter of second word must be capitalized.
        object <- gsub("\\.([[:alpha:]])", "\\U\\1", object, perl = TRUE)

        ## Remaining dots should be sanitized with "X" character.
        pattern <- "\\."
        if (any(grepl(pattern, object))) {
            if (format == "lower") {
                replacement <- "x"
            } else if (format == "upper") {
                replacement <- "X"
            }
            object <- gsub(pattern, replacement, object)
        }

        object
    }



## Base R classes ===============================================================
## Updated 2019-07-19.
`camelCase,atomic` <-  # nolint
    function(object, names = TRUE, strict = FALSE) {
        validObject(object)
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- camelCase(names(object), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("atomic"),
    definition = `camelCase,atomic`
)



## Updated 2019-07-19.
`camelCase,character` <-  # nolint
    function(object, names = TRUE, strict = FALSE) {
        validObject(object)
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .camelCase(names(object), strict = strict)
        } else {
            names <- names(object)
        }
        object <- .camelCase(object, strict = strict)
        names(object) <- names
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("character"),
    definition = `camelCase,character`
)



## Updated 2019-07-19.
`camelCase,factor` <-  # nolint
    function(object, names = TRUE, strict = FALSE) {
        validObject(object)
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .camelCase(names(object), strict = strict)
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- camelCase(object, strict = strict)
        object <- as.factor(object)
        names(object) <- names
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("factor"),
    definition = `camelCase,factor`
)



## Updated 2019-07-19.
`camelCase,list` <- `camelCase,atomic`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("list"),
    definition = `camelCase,list`
)



## Updated 2019-07-19.
`camelCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        validObject(object)
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("matrix"),
    definition = `camelCase,matrix`
)



## Updated 2019-07-19.
`camelCase,data.frame` <- `camelCase,matrix`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("data.frame"),
    definition = `camelCase,data.frame`
)



## S4 virtual classes ===========================================================
## Updated 2019-07-19.
`camelCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = FALSE
    ) {
        validObject(object)
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <-
                camelCase(names(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            mcolnames(object) <-
                camelCase(mcolnames(object), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Vector"),
    definition = `camelCase,Vector`
)



## Updated 2019-07-19.
## mcols metadata
`camelCase,DataTable` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = FALSE
    ) {
        validObject(object)
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            mcolnames(object) <-
                camelCase(mcolnames(object), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object

    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("DataTable"),
    definition = `camelCase,DataTable`
)



## Updated 2019-07-19.
`camelCase,Ranges` <- `camelCase,Vector`  # nolint
formals(`camelCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Ranges"),
    definition = `camelCase,Ranges`
)



## Updated 2019-07-19.
`camelCase,Matrix` <- `camelCase,matrix`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Matrix"),
    definition = `camelCase,Matrix`
)



## S4 classes ===================================================================
## Updated 2019-07-19.
`camelCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE,
        strict = FALSE
    ) {
        validObject(object)
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <-
                camelCase(names(assays(object)), strict = strict)
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <-
                     camelCase(colnames(rowData(object)), strict = strict)
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <-
                camelCase(colnames(colData(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("SummarizedExperiment"),
    definition = `camelCase,SummarizedExperiment`
)



## Aliases ======================================================================
#' @rdname camelCase
#' @usage NULL
#' @export
camel <- function(...) {
    camelCase(...)
}
