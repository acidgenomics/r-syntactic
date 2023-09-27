test_that("makeTitle", {
    expect_identical(
        object = vapply(
            X = mw,
            FUN = makeTitle,
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        ),
        expected = c(
            "Log10 genes per UMI",
            "Mito vs. coding",
            "Words already",
            "NASA",
            "N gene"
        )
    )
})
