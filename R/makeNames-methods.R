#' @name makeNames
#' @inherit AcidGenerics::makeNames
#' @note Updated 2026-06-03.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @seealso
#' - [ASCII table](https://cs.stanford.edu/people/miles/iso8859.html)
#' - `chartr` base R function for single character substitutions.
#' - `stringi::stri_trans_general` with `id = "Latin-ASCII"` for automatic
#' decoding of characters with accent marks.
#' - https://stackoverflow.com/questions/17517319
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic[["character"]]
#' makeNames(object)
NULL


## Updated 2026-06-03.
`makeNames,character` <- # nolint
    function(object, unique = TRUE, smart = FALSE) {
        assert(
            isFlag(unique),
            isFlag(smart)
        )
        x <- as.character(object)
        assert(all(nzchar(x, keepNA = FALSE)))
        ## Single character substitutions (Latin-1 supplement, U+00C0-U+00FF).
        x <- chartr(
            old = paste0(
                "\u00C0\u00C1\u00C2\u00C3\u00C4\u00C5",
                "\u00C7",
                "\u00C8\u00C9\u00CA\u00CB",
                "\u00CC\u00CD\u00CE\u00CF",
                "\u00D0\u00D1",
                "\u00D2\u00D3\u00D4\u00D5\u00D6",
                "\u00D7",
                "\u00D8",
                "\u00D9\u00DA\u00DB\u00DC",
                "\u00DD",
                "\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5",
                "\u00E7",
                "\u00E8\u00E9\u00EA\u00EB",
                "\u00EC\u00ED\u00EE\u00EF",
                "\u00F0\u00F1",
                "\u00F2\u00F3\u00F4\u00F5\u00F6",
                "\u00F7",
                "\u00F8",
                "\u00F9\u00FA\u00FB\u00FC",
                "\u00FD\u00FF"
            ),
            new = paste0(
                "AAAAAA",
                "C",
                "EEEE",
                "IIII",
                "DN",
                "OOOOO",
                "*",
                "O",
                "UUUU",
                "Y",
                "aaaaaa",
                "c",
                "eeee",
                "iiii",
                "dn",
                "ooooo",
                "/",
                "o",
                "uuuu",
                "yy"
            ),
            x = x
        )
        ## Multi-character substitutions.
        x <- gsub(
            pattern = "\u00C6",
            replacement = "AE",
            x = x,
            fixed = TRUE
        )
        x <- gsub(
            pattern = "\u00DE",
            replacement = "TH",
            x = x,
            fixed = TRUE
        )
        x <- gsub(
            pattern = "\u00DF",
            replacement = "ss",
            x = x,
            fixed = TRUE
        )
        x <- gsub(
            pattern = "\u00E6",
            replacement = "ae",
            x = x,
            fixed = TRUE
        )
        x <- gsub(
            pattern = "\u00FE",
            replacement = "th",
            x = x,
            fixed = TRUE
        )
        ## Coerce pesky "micro" characters to "u".
        x <- gsub(
            pattern = "(\u00B5|\u03BC|&#181;)",
            replacement = "u",
            x = x,
            fixed = FALSE
        )
        ## Greek letters (U+0391-U+03C9).
        ## Note: U+03BC (Greek small letter mu) is handled above as micro sign.
        greekMap <- c(
            "\u0391" = "Alpha",
            "\u0392" = "Beta",
            "\u0393" = "Gamma",
            "\u0394" = "Delta",
            "\u0395" = "Epsilon",
            "\u0396" = "Zeta",
            "\u0397" = "Eta",
            "\u0398" = "Theta",
            "\u0399" = "Iota",
            "\u039A" = "Kappa",
            "\u039B" = "Lambda",
            "\u039C" = "Mu",
            "\u039D" = "Nu",
            "\u039E" = "Xi",
            "\u039F" = "Omicron",
            "\u03A0" = "Pi",
            "\u03A1" = "Rho",
            "\u03A3" = "Sigma",
            "\u03A4" = "Tau",
            "\u03A5" = "Upsilon",
            "\u03A6" = "Phi",
            "\u03A7" = "Chi",
            "\u03A8" = "Psi",
            "\u03A9" = "Omega",
            "\u03B1" = "alpha",
            "\u03B2" = "beta",
            "\u03B3" = "gamma",
            "\u03B4" = "delta",
            "\u03B5" = "epsilon",
            "\u03B6" = "zeta",
            "\u03B7" = "eta",
            "\u03B8" = "theta",
            "\u03B9" = "iota",
            "\u03BA" = "kappa",
            "\u03BB" = "lambda",
            "\u03BC" = "mu",
            "\u03BD" = "nu",
            "\u03BE" = "xi",
            "\u03BF" = "omicron",
            "\u03C0" = "pi",
            "\u03C1" = "rho",
            "\u03C2" = "sigma",
            "\u03C3" = "sigma",
            "\u03C4" = "tau",
            "\u03C5" = "upsilon",
            "\u03C6" = "phi",
            "\u03C7" = "chi",
            "\u03C8" = "psi",
            "\u03C9" = "omega"
        )
        for (i in seq_along(greekMap)) {
            x <- gsub(
                pattern = names(greekMap)[[i]],
                replacement = greekMap[[i]],
                x = x,
                fixed = TRUE
            )
        }
        if (isTRUE(smart)) {
            x <- gsub(
                pattern = "'",
                replacement = "",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "&",
                replacement = "_and_",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "+",
                replacement = "_plus_",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "[[:space:]]-[[:space:]]",
                replacement = " ",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "-[[:space:]]",
                replacement = "_minus_",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "^-(.+)$",
                replacement = "minus_\\1",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "^(.+)-$",
                replacement = "\\1_minus",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "%",
                replacement = "_percent_",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "*",
                replacement = "_times_",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "/",
                replacement = "_slash_",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "(\\d),(\\d)",
                replacement = "\\1\\2",
                x = x,
                fixed = FALSE
            )
        }
        x <- gsub(
            pattern = "[^A-Za-z0-9]",
            replacement = "_",
            x = x,
            fixed = FALSE
        )
        x <- gsub(
            pattern = "(^_|_$)",
            replacement = "",
            x = x,
            fixed = FALSE
        )
        x <- make.names(names = x, unique = unique, allow_ = TRUE)
        x <- gsub(
            pattern = "\\.",
            replacement = "_",
            x = x,
            fixed = FALSE
        )
        x <- gsub(
            pattern = "[_]+",
            replacement = "_",
            x = x,
            fixed = FALSE
        )
        x <- gsub(
            pattern = "(^_|_$)",
            replacement = "",
            x = x,
            fixed = FALSE
        )
        x <- gsub(
            pattern = "^NA$",
            replacement = NA_character_,
            x = x,
            fixed = FALSE
        )
        if (isTRUE(unique)) {
            assert(hasNoDuplicates(x))
        }
        assert(all(nzchar(x, keepNA = FALSE)))
        x
    }


## Updated 2023-04-12.
`makeNames,factor` <- # nolint
    `makeNames,character`


#' @rdname makeNames
#' @export
setMethod(
    f = "makeNames",
    signature = signature(object = "character"),
    definition = `makeNames,character`
)

#' @rdname makeNames
#' @export
setMethod(
    f = "makeNames",
    signature = signature(object = "factor"),
    definition = `makeNames,factor`
)
