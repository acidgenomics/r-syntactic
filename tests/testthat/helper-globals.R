data(
    DataFrame,
    GRanges,
    SummarizedExperiment,
    syntactic,
    package = "acidtest",
    envir = environment()
)

df <- DataFrame
se <- SummarizedExperiment
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
