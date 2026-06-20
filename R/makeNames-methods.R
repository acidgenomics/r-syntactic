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
        ## setNames avoids Unicode-escaped named-vector literals which crash
        ## lintr 3.3's parser (github.com/r-lib/lintr/issues/2507).
        greekMap <- setNames(
            object = c(
                "Alpha", "Beta", "Gamma", "Delta", "Epsilon",
                "Zeta", "Eta", "Theta", "Iota", "Kappa",
                "Lambda", "Mu", "Nu", "Xi", "Omicron",
                "Pi", "Rho", "Sigma", "Tau", "Upsilon",
                "Phi", "Chi", "Psi", "Omega",
                "alpha", "beta", "gamma", "delta", "epsilon",
                "zeta", "eta", "theta", "iota", "kappa",
                "lambda", "mu", "nu", "xi", "omicron",
                "pi", "rho", "sigma", "sigma", "tau",
                "upsilon", "phi", "chi", "psi", "omega"
            ),
            nm = c(
                "\u0391", "\u0392", "\u0393", "\u0394", "\u0395",
                "\u0396", "\u0397", "\u0398", "\u0399", "\u039A",
                "\u039B", "\u039C", "\u039D", "\u039E", "\u039F",
                "\u03A0", "\u03A1", "\u03A3", "\u03A4", "\u03A5",
                "\u03A6", "\u03A7", "\u03A8", "\u03A9",
                "\u03B1", "\u03B2", "\u03B3", "\u03B4", "\u03B5",
                "\u03B6", "\u03B7", "\u03B8", "\u03B9", "\u03BA",
                "\u03BB", "\u03BC", "\u03BD", "\u03BE", "\u03BF",
                "\u03C0", "\u03C1", "\u03C2", "\u03C3", "\u03C4",
                "\u03C5", "\u03C6", "\u03C7", "\u03C8", "\u03C9"
            )
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
            pattern = ".",
            replacement = "_",
            x = x,
            fixed = TRUE
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
