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
