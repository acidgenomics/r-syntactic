context("label")

test_that("label", {
    expect_identical(
        object = vapply(
            X = mw,
            FUN = label,
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        ),
        expected = c(
            "kill v. maim",
            "log10 genes per UMI",
            "mito vs. coding",
            "words already",
            "NASA"
        )
    )
})
