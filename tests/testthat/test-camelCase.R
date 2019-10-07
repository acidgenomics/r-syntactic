context("camelCase")

with_parameters_test_that(
    "Strict mode", {
        expect_identical(
            object = f(vec, strict = TRUE),
            expected = expected
        )
    },
    f = list(camelCase, upperCamelCase),
    expected = list(
        camelCase = c(
            "percentGc",
            "x10um",
            "x5x3Bias",
            "x5prime",
            "g2mScore",
            "helloWorld",
            "helloWorld",
            "mazdaRx4",
            "nCount",
            "rnaiClones",
            "tx2gene",
            "tx2GeneId",
            "worfdbHtmlRemap",
            "x123",
            NA
        ),
        upperCamelCase = c(
            "PercentGc",
            "X10um",
            "X5X3Bias",
            "X5prime",
            "G2mScore",
            "HelloWorld",
            "HelloWorld",
            "MazdaRx4",
            "NCount",
            "RnaiClones",
            "Tx2gene",
            "Tx2GeneId",
            "WorfdbHtmlRemap",
            "X123",
            NA
        )
    )
)

test_that("Disable X prefix", {
    object <- c("1" = "1 foo bar")
    expect_identical(
        object = camelCase(object),
        expected = c("x1" = "x1FooBar")
    )
    expect_identical(
        object = camelCase(object, prefix = FALSE),
        expected = c("x1" = "1FooBar")
    )
})
