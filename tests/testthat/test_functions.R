data(mn, package = "acidtest", envir = environment())



context("atomic")

vec <- mn[["character"]]

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

test_that("capitalize", {
    expect_identical(
        object = capitalize(vec),
        expected = c(
            "Hello world",
            "HELLO WORLD",
            "RNAi clones",
            "NCount",
            "Tx2gene",
            "TX2GeneID",
            "G2M.Score",
            "WorfdbHTMLRemap",
            "Mazda RX4",
            "%GC",
            "5prime",
            "5'-3' bias",
            "123",
            NA_character_
        )
    )
})

test_that("kebab", {
    expect_identical(
        object = kebab(vec),
        expected = c(
            "hello-world",
            "hello-world",
            "rnai-clones",
            "n-count",
            "tx2gene",
            "tx2-gene-id",
            "g2m-score",
            "worfdb-html-remap",
            "mazda-rx4",
            "percent-gc",
            "x5prime",
            "x5-3-bias",
            "x123",
            NA_character_
        )
    )
})



context("matrix")

test_that("makeDimnames", {
    mat <- matrix(
        data = seq_len(4L),
        nrow = 2L,
        dimnames = list(
            c("1-a", "1-a"),
            c("2-b", "2-b")
        )
    )
    expect_identical(
        object = dimnames(makeDimnames(mat)),
        expected = list(
            c("X1_a", "X1_a_1"),
            c("X2_b", "X2_b_1")
        )
    )
})
