#' @name camelCase
#' @inherit AcidGenerics::camelCase
#' @note Updated 2021-01-21.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic$character
#' camelCase(object)
NULL



## Updated 2021-03-03.
.camelCase <-  # nolint
    function(
        x,
        format = c("lower", "upper"),
        strict = TRUE,
        ...
    ) {
        assert(isFlag(strict))
        format <- match.arg(format)
        x <- .syntactic(x, ...)
        ## Dots but not underscores define word boundaries in grep matches.
        x <- gsub(pattern = "_", replacement = ".", x = x)
        if (isTRUE(strict)) {
            ## Simplify mixed case acronyms in strict mode.
            x <- tolower(x)
            ## Characters following numbers are considered new words in this
            ## mode, which is unlike other settings in package.
            ## > x <- gsub(
            ## >     pattern = "([0-9]+)([a-z]+)",
            ## >     replacement = "\\1.\\2",
            ## >     x = x
            ## > )
        }
        ## lowerCamelCase or UpperCamelCase.
        if (identical(format, "lower")) {
            ## lowerCamelCase
            ## Coerce first word to lower.
            x <- gsub(
                pattern = "^(\\w+)\\b",
                replacement = "\\L\\1",
                x = x,
                perl = TRUE
            )
        } else if (identical(format, "upper")) {
            ## UpperCamelCase
            ## Capitalize the first letter.
            x <- gsub(
                pattern = "^([a-z])",
                replacement = "\\U\\1",
                x = x,
                perl = TRUE
            )
        }
        ## Remove dots in between numbers following a letter.
        x <- gsub("([[:alpha:]])\\.([[:digit:]])", "\\1\\2", x)
        ## First letter of second word must be capitalized.
        x <- gsub("\\.([[:alpha:]])", "\\U\\1", x, perl = TRUE)
        ## Remaining dots should be sanitized with "X" character.
        pattern <- "\\."
        if (any(grepl(pattern, x))) {
            if (identical(format, "lower")) {
                replacement <- "x"
            } else if (identical(format, "upper")) {
                replacement <- "X"
            }
            x <- gsub(pattern, replacement, x)
        }
        x
    }



`camelCase,character` <-  # nolint
    function(
        object,
        rename = FALSE,
        recursive = FALSE,
        smart = TRUE,
        strict = TRUE,
        names = !rename,
        prefix = !rename
    ) {
        assert(
            isCharacter(object),
            isFlag(rename),
            isFlag(recursive),
            isFlag(smart),
            isFlag(strict),
            isFlag(names),
            isFlag(prefix)
        )
        ## Rename mode ---------------------------------------------------------
        ## FIXME Take this out and move to koopa package.
        if (isTRUE(rename)) {
            path <- .rename(
                path = object,
                recursive = recursive,
                fun = "camelCase",
                smart = smart,
                strict = strict,
                prefix = prefix
            )
            return(invisible(path))
        }
        ## String mode ---------------------------------------------------------
        if (isTRUE(names) && hasNames(object)) {
            names <- .camelCase(
                x = names(object),
                strict = strict,
                prefix = TRUE,
                smart = smart
            )
        } else {
            names <- names(object)
        }
        object <- .camelCase(
            x = object,
            strict = strict,
            prefix = prefix,
            smart = smart
        )
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
