context("upperCamel")

test_that("Strict mode", {
    expect_identical(
        object = upperCamelCase(vec, strict = TRUE),
        expected = c(
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
})

test_that("Disable X prefix", {
    object <- c("1" = "1 foo bar")
    expect_identical(
        object = upperCamelCase(object),
        expected = c("X1" = "X1FooBar")
    )
    expect_identical(
        object = upperCamelCase(object, prefix = FALSE),
        expected = c("X1" = "1FooBar")
    )
})
