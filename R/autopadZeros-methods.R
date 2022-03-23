#' @name autopadZeros
#' @inherit AcidGenerics::autopadZeros
#'
#' @note Updated 2021-08-24.
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



## Updated 2020-06-15.
`autopadZeros,integer` <- # nolint
    function(object) {
        names <- names(object)
        object <- as.character(object)
        object <- autopadZeros(object)
        names(object) <- names
        object
    }



## Updated 2021-08-24.
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
            return(object)
        }
        if (isTRUE(int)) {
            num <- x
        } else {
            match <- str_match(string = x, pattern = dict[["pattern"]])
            if (identical(dict[["side"]], "left")) {
                colnames(match) <- c("string", "num", "stem")
            } else if (identical(dict[["side"]], "right")) {
                colnames(match) <- c("string", "stem", "num")
            }
            num <- match[, "num"]
        }
        width <- max(str_length(num))
        num <- str_pad(string = num, width = width, side = "left", pad = "0")
        if (isTRUE(int)) {
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
