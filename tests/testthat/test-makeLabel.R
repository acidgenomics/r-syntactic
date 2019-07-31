context("makeLabel")

test_that("makeLabel", {
    expect_identical(
        object = vapply(
            X = mw,
            FUN = makeLabel,
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        ),
        expected = c(
            "kill v. maim",
            "log10 genes per UMI",
            "mito vs. coding",
            "words already",
            "NASA",
            "n gene"
        )
    )
})
