#' @name autopadZeros
#' @inherit acidgenerics::autopadZeros
#'
#' @note Updated 2020-06-15.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @return `character`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#'
#' ## integer ====
#' autopadZeros(c(1L, 10L, 100L))
#'
#' ## character ====
#' ## Left side.
#' autopadZeros(c("1-EV-DMSO", "10-EV-DMSO", "2-EV-DMSO-B"))
#' ## Right side.
#' autopadZeros(c("A1", "B10", "C100"))
NULL



#' @rdname autopadZeros
#' @name autopadZeros
#' @importFrom acidgenerics autopadZeros
#' @usage autopadZeros(object, ...)
#' @export
NULL



## Updated 2019-09-05.
`autopadZeros,character` <-  # nolint
    function(object) {
        x <- unname(object)
        ## Detect if we need to pad the left or right side automatically.
        leftPattern <- "^([[:digit:]]+)(.+)$"
        rightPattern <- "^([^0-9]+)([[:digit:]]+)$"
        if (allAreMatchingRegex(x = x, pattern = leftPattern)) {
            side <- "left"
            pattern <- leftPattern
        } else if (allAreMatchingRegex(x = x, pattern = rightPattern)) {
            side <- "right"
            pattern <- rightPattern
        } else {
            ## Early return if no padding is necessary.
            return(object)
        }
        match <- str_match(string = x, pattern = pattern)
        if (identical(side, "left")) {
            colnames(match) <- c("string", "num", "stem")
        } else if (identical(side, "right")) {
            colnames(match) <- c("string", "stem", "num")
        }
        num <- match[, "num"]
        width <- max(str_length(num))
        num <- str_pad(string = num, width = width, side = "left", pad = "0")
        stem <- match[, "stem"]
        if (identical(side, "left")) {
            x <- paste0(num, stem)
        } else if (identical(side, "right")) {
            x <- paste0(stem, num)
        }
        names(x) <- names(object)
        x
    }



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature("character"),
    definition = `autopadZeros,character`
)
