test_that("makeWords", {
    expect_identical(
        object = makeWords(mw),
        expected = c(
            "log10 genes per UMI",
            "mito vs. coding",
            "words already",
            "NASA",
            "n gene"
        )
    )
})
