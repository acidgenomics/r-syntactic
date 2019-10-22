context("capitalize")

test_that("Vectorized", {
    object <- c(
        "the quick Brown fox",
        "using AIC for model selection",
        "NASA"
    )
    expect_identical(
        object = capitalize(object, strict = FALSE),
        expected = c(
            "The Quick Brown Fox",
            "Using AIC For Model Selection",
            "NASA"
        )
    )
    expect_identical(
        object = capitalize(object, strict = TRUE),
        expected = c(
            "The Quick Brown Fox",
            "Using Aic For Model Selection",
            "Nasa"
        )
    )
})
