test_that("makeNames", {
    expect_identical(
        object = makeNames(vec),
        expected = c(
            "GC",
            "X10uM",
            "X5_3_bias",
            "X5prime",
            "G2M_Score",
            "hello_world",
            "HELLO_WORLD",
            "Mazda_RX4",
            "nCount",
            "RNAi_clones",
            "tx2gene",
            "TX2GeneID",
            "worfdbHTMLRemap",
            "X123"
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
        object = makeNames(as.character(c(1L, 1L)), unique = TRUE),
        expected = c("X1", "X1_1")
    )
    expect_identical(
        object = makeNames(
            object = c(
                "a - b",
                "a- b",
                "a -b",
                "a+b",
                "a/b",
                "a&b",
                "a'b",
                "a,b",
                "1,0"
            ),
            unique = FALSE,
            smart = TRUE
        ),
        expected = c(
            "a_b",
            "a_minus_b",
            "a_b",
            "a_plus_b",
            "a_slash_b",
            "a_and_b",
            "ab",
            "a_b",
            "X10"
        )
    )
})
