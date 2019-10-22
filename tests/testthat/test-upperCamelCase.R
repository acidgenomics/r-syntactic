context("upperCamelCase")

test_that("Unnamed character", {
    expect_identical(
        object = upperCamelCase(unnamed),
        expected = c(
            "PercentGC",
            "X10um",
            "X5X3Bias",
            "X5prime",
            "G2MScore",
            "HelloWorld",
            "HELLOWORLD",
            "MazdaRX4",
            "NCount",
            "RNAIClones",
            "Tx2gene",
            "TX2GeneID",
            "WorfdbHTMLRemap",
            "X123"
        )
    )
})

test_that("Named character", {
    expect_identical(
        object = upperCamelCase(named, names = TRUE),
        expected = c(
            ItemA = "HelloWorld",
            ItemB = "HELLOWORLD"
        )
    )
    expect_identical(
        object = names(camelCase(named, names = FALSE)),
        expected = names(named)
    )
})

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
            "X123"
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
