context("kebabCase")

test_that("kebabCase", {
    expect_identical(
        object = kebabCase(vec),
        expected = c(
            "percent-gc",
            "x10um",
            "x5-3-bias",
            "x5prime",
            "g2m-score",
            "hello-world",
            "hello-world",
            "mazda-rx4",
            "n-count",
            "rnai-clones",
            "tx2gene",
            "tx2-gene-id",
            "worfdb-html-remap",
            "x123",
            NA_character_
        )
    )
})
