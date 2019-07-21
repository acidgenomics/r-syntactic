context("kebabCase")

test_that("kebabCase", {
    expect_identical(
        object = kebabCase(vec),
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
