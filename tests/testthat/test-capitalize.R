context("capitalize")

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
