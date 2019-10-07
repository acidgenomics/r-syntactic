data(syntactic, package = "acidtest", envir = environment())
vec <- syntactic[["character"]]
funs <- list(
    camelCase = camelCase,
    dottedCase = dottedCase,
    snakeCase = snakeCase,
    upperCamelCase = upperCamelCase
)

unnamed <- syntactic[["character"]]
named <- syntactic[["character_named"]]

## Delimited numbers.
dn <- c(
    "1,000,000",
    "0.01",
    "2018-01-01",
    "res.0.1"
)
## makeWords check vector.
mw <- c(
    "killVMaim",
    "log10GenesPerUMI",
    "mitoVsCoding",
    "words already",
    "NASA",
    "nGene"
)
## Plus minus testing.
pm <- c(
    "100%",
    "+/-",
    "a +/- b",
    "dox-",
    "dox+",
    "-dox",
    "+dox",
    "/",
    "-"
)
