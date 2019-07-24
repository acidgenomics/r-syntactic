data(
    DataFrame,
    GRanges,
    RangedSummarizedExperiment,
    syntactic,
    package = "acidtest",
    envir = environment()
)

df <- DataFrame
gr <- GRanges
rse <- RangedSummarizedExperiment
vec <- syntactic[["character"]]

funs <- list(
    camelCase = camelCase,
    dottedCase = dottedCase,
    snakeCase = snakeCase,
    upperCamelCase = upperCamelCase
)

## nolint start
DataFrame <- S4Vectors::DataFrame
## nolint end
