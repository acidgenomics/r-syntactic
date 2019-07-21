context("capitalize")

test_that("capitalize", {
    expect_identical(
        object = capitalize(vec),
        expected = c(
            "%GC",
            "10uM",
            "5'-3' bias",
            "5prime",
            "G2M.Score",
            "Hello world",
            "HELLO WORLD",
            "Mazda RX4",
            "NCount",
            "RNAi clones",
            "Tx2gene",
            "TX2GeneID",
            "WorfdbHTMLRemap",
            "123",
            NA_character_
        )
    )
})
