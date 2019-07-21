context("makeNames")

test_that("makeNames", {
    expect_identical(
        object = makeNames(vec),
        expected = c(
            "hello_world",
            "HELLO_WORLD",
            "RNAi_clones",
            "nCount",
            "tx2gene",
            "TX2GeneID",
            "G2M_Score",
            "worfdbHTMLRemap",
            "Mazda_RX4",
            "X_GC",
            "X5prime",
            "X5__3__bias",
            "X123",
            "NA_"
        )
    )
    expect_identical(
        object = makeNames(c("a-b", "a-b"), unique = TRUE),
        expected = c("a_b", "a_b_1")
    )
    expect_identical(
        object = makeNames(c("a-b", "a-b"), unique = FALSE),
        expected = c("a_b", "a_b")
    )
    expect_identical(
        object = makeNames(c(1L, 1L), unique = TRUE),
        expected = c("X1", "X1_1")
    )
})
