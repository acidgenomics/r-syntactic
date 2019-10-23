context("snakeCase")

test_that("Unnamed character", {
    expect_identical(
        object = snakeCase(unnamed),
        expected = c(
            "percent_gc",
            "x10um",
            "x5_3_bias",
            "x5prime",
            "g2m_score",
            "hello_world",
            "hello_world",
            "mazda_rx4",
            "n_count",
            "rnai_clones",
            "tx2gene",
            "tx2_gene_id",
            "worfdb_html_remap",
            "x123"
        )
    )
})

test_that("Named character", {
    expect_identical(
        object = snakeCase(named, names = TRUE),
        expected = c(
            "item_a" = "hello_world",
            "item_b" = "hello_world"
        )
    )
    expect_identical(
        object = names(camelCase(named, names = FALSE)),
        expected = names(named)
    )
})

test_that("Plus minus handling", {
    expect_identical(
        object = snakeCase(pm),
        expected = c(
            "x100_percent",
            "plus_slash_minus",
            "a_plus_slash_minus_b",
            "dox_minus",
            "dox_plus",
            "x_dox",
            "plus_dox",
            "slash",
            "x"
        )
    )
})

test_that("File rename mode", {
    topdir <- "XXX"
    unlink(topdir, recursive = TRUE)
    dirs <- file.path(topdir, c("aaa-bbb", "ccc-ddd"))
    files <- file.path(topdir, c("1-sample-A.fastq.gz", "hello-world.txt"))
    input <- c(files, dirs)
    lapply(dirs, dir.create, recursive = TRUE)
    file.create(files)
    output <- snakeCase(input, rename = TRUE)
    expect_identical(
        object = output,
        expected = file.path(
            topdir,
            c(
                "1_sample_a.fastq.gz",
                "hello_world.txt",
                "aaa_bbb",
                "ccc_ddd"
            )
        )
    )
    unlink(topdir, recursive = TRUE)
})
