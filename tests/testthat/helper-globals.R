data(
    DataFrame,
    GRanges,
    syntactic,
    package = "acidtest",
    envir = environment()
)

funs <- list(
    camelCase = camelCase,
    dottedCase = dottedCase,
    snakeCase = snakeCase,
    upperCamelCase = upperCamelCase
)
df <- DataFrame
vec <- syntactic[["character"]]

## nolint start
DataFrame <- S4Vectors::DataFrame
## nolint end
