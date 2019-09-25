data(syntactic, package = "acidtest", envir = environment())
vec <- syntactic[["character"]]
funs <- list(
    camelCase = camelCase,
    dottedCase = dottedCase,
    snakeCase = snakeCase,
    upperCamelCase = upperCamelCase
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
