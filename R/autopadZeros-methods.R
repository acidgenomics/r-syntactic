#' @name autopadZeros
#' @inherit AcidGenerics::autopadZeros
#'
#' @note Updated 2023-09-21.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `character`.
#'
#' @seealso
#' - `stringr::str_pad`.
#'
#' @examples
#' ## integer ====
#' autopadZeros(c(1L, 10L, 100L))
#'
#' ## character ====
#' ## Left side.
#' autopadZeros(c("1-EV-DMSO", "10-EV-DMSO", "2-EV-DMSO-B"))
#' ## Right side.
#' autopadZeros(c("A1", "B10", "C100"))
NULL



#' Extract components from a match
#'
#' @note Updated 2023-09-21
#' @noRd
#'
#' @return `matrix`.
#' Character matrix of match groups.
#'
#' @seealso
#' - `stringr::str_match`.
#' - https://stringr.tidyverse.org/articles/from-base.html
.strMatch <- function(x, pattern) {
    m <- regexec(pattern = pattern, text = x)
    l <- regmatches(x = x, m = m)
    mat <- do.call(what = rbind, args = l)
    mat
}



## Updated 2020-06-15.
`autopadZeros,integer` <- # nolint
    function(object) {
        names <- names(object)
        object <- as.character(object)
        object <- autopadZeros(object)
        names(object) <- names
        object
    }



## Updated 2023-09-21.
`autopadZeros,character` <- # nolint
    function(object) {
        x <- unname(object)
        dict <- list(
            "int" = FALSE,
            "intPattern" = "^([[:digit:]]+)$",
            "leftPattern" = "^([[:digit:]]+)(.+)$",
            "rightPattern" = "^(.*[^0-9]+)([[:digit:]]+)$"
        )
        if (allAreMatchingRegex(x = x, pattern = dict[["intPattern"]])) {
            dict[["int"]] <- TRUE
        } else if (
            allAreMatchingRegex(x = x, pattern = dict[["leftPattern"]])
        ) {
            dict[["side"]] <- "left"
            dict[["pattern"]] <- dict[["leftPattern"]]
        } else if (
            allAreMatchingRegex(x = x, pattern = dict[["rightPattern"]])
        ) {
            dict[["side"]] <- "right"
            dict[["pattern"]] <- dict[["rightPattern"]]
        } else {
            assert(
                !any(c(
                    isMatchingRegex(x = x, pattern = dict[["intPattern"]]),
                    isMatchingRegex(x = x, pattern = dict[["leftPattern"]]),
                    isMatchingRegex(x = x, pattern = dict[["rightPattern"]])
                )),
                msg = "Partial padding match detected."
            )
            return(object)
        }
        if (isTRUE(dict[["int"]])) {
            num <- x
        } else {
            match <- .strMatch(x = x, pattern = dict[["pattern"]])
            if (identical(dict[["side"]], "left")) {
                colnames(match) <- c("string", "num", "stem")
            } else if (identical(dict[["side"]], "right")) {
                colnames(match) <- c("string", "stem", "num")
            }
            num <- match[, "num"]
        }
        ## FIXME Rework using base R.
        width <- max(stringr::str_length(num))
        ## FIXME Rework using base R.
        num <- stringr::str_pad(
            string = num,
            width = width,
            side = "left",
            pad = "0"
        )
        if (isTRUE(dict[["int"]])) {
            x <- num
        } else {
            stem <- match[, "stem"]
            if (identical(dict[["side"]], "left")) {
                x <- paste0(num, stem)
            } else if (identical(dict[["side"]], "right")) {
                x <- paste0(stem, num)
            }
        }
        names(x) <- names(object)
        x
    }



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature(object = "character"),
    definition = `autopadZeros,character`
)

#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature(object = "integer"),
    definition = `autopadZeros,integer`
)
