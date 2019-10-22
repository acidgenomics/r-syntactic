context("kebabCase")

test_that("Unnamed character", {
    expect_identical(
        object = kebabCase(unnamed),
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
            "x123"
        )
    )
})

test_that("Disable X prefix", {
    object <- c("1" = "1 foo bar")
    expect_identical(
        object = kebabCase(object),
        expected = c("1" = "x1-foo-bar")
    )
    expect_identical(
        object = kebabCase(object, prefix = FALSE),
        c("1" = "1-foo-bar")
    )
})

test_that("File rename mode", {
    topdir <- "XXX"
    unlink(topdir, recursive = TRUE)
    dirs <- file.path(topdir, c("aaa_bbb", "ccc_ddd"))
    files <- file.path(topdir, c("1_sample_A.fastq.gz", "hello_world.txt"))
    input <- c(files, dirs)
    lapply(dirs, dir.create, recursive = TRUE)
    file.create(files)
    output <- kebabCase(input, rename = TRUE, prefix = FALSE)
    expect_identical(
        object = output,
        expected = file.path(
            topdir,
            c(
                "1-sample-a.fastq.gz",
                "hello-world.txt",
                "aaa-bbb",
                "ccc-ddd"
            )
        )
    )
    unlink(topdir, recursive = TRUE)
})
