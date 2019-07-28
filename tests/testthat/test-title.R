context("title")

test_that("title", {
    expect_identical(
        object = vapply(
            X = mw,
            FUN = title,
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        ),
        expected = c(
            "Kill v. maim",
            "Log10 genes per UMI",
            "Mito vs. coding",
            "Words already",
            "NASA"
        )
    )
})
