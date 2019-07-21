data(
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
gr <- GRanges
vec <- syntactic[["character"]]
