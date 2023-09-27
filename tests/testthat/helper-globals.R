data(syntactic, package = "AcidTest", envir = environment())

named <- syntactic[["character_named"]]
unnamed <- syntactic[["character"]]
vec <- syntactic[["character"]]

## Delimited numbers.
dn <- c(
    "1,000,000",
    "0.01",
    "2018-01-01",
    "res.0.1"
)
## makeWords check vector.
mw <- c(
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

initDir <- AcidBase::initDir
tempdir2 <- AcidBase::tempdir2
unlink2 <- AcidBase::unlink2
