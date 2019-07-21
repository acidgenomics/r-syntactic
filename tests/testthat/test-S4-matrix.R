context("matrix")

with_parameters_test_that(
    "matrix", {
        object <- syntactic[["matrix"]][1L:3L, 1L:3L]
        expect_identical(
            object = dimnames(f(object, rownames = TRUE, colnames = TRUE)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camelCase = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urbanPop")
        ),
        dottedCase = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "Urban.Pop")
        ),
        snakeCase = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urban_pop")
        ),
        upperCamelCase = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "UrbanPop")
        )
    )
)
