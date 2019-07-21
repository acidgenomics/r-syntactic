context("camelCase")

with_parameters_test_that(
    "Strict mode", {
        x <- syntactic[["character"]]
        expect_identical(
            object = f(x, strict = TRUE),
            expected = expected
        )
    },
    f = list(camelCase, upperCamelCase),
    expected = list(
        camelCase = c(
            "helloWorld",
            "helloWorld",
            "rnaiClones",
            "nCount",
            "tx2gene",
            "tx2GeneId",
            "g2mScore",
            "worfdbHtmlRemap",
            "mazdaRx4",
            "percentGc",
            "x5prime",
            "x5x3Bias",
            "x123",
            NA
        ),
        upperCamelCase = c(
            "HelloWorld",
            "HelloWorld",
            "RnaiClones",
            "NCount",
            "Tx2gene",
            "Tx2GeneId",
            "G2mScore",
            "WorfdbHtmlRemap",
            "MazdaRx4",
            "PercentGc",
            "X5prime",
            "X5X3Bias",
            "X123",
            NA
        )
    )
)
