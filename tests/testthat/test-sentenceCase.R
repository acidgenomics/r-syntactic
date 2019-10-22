context("sentenceCase")

test_that("Vectorized", {
    expect_identical(
        object = sentenceCase(
            object = c(
                "the quick Brown fox",
                "using AIC for model selection",
                "NASA"
            )
        ),
        expected = c(
            "The quick brown fox",
            "Using AIC for model selection",
            "NASA"
        )
    )
})
