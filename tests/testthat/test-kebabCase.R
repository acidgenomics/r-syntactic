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

test_that("Rename mode", {
    topdir <- "XXX"
    unlink(topdir, recursive = TRUE)
    topdir <- initDir(topdir)
    dirs <- file.path(topdir, c("aaa_bbb", "ccc_ddd"))
    files <- file.path(topdir, c("1_sample_A.fastq.gz", "hello_world.txt"))
    input <- c(files, dirs)
    lapply(dirs, dir.create, recursive = TRUE)
    file.create(files)
    output <- kebabCase(input, rename = TRUE)
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

test_that("Rename mode (recursive)", {
    path <- file.path(tempdir(), "recursive")
    unlink(path, recursive = TRUE)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    dir.create(
        path = file.path(path, "level_1", "level_2"),
        recursive = TRUE
    )
    file.create(
        file.path(path, "file_x.TXT"),
        file.path(path, "level_1", "file_x.txt"),
        file.path(path, "level_1", "level_2", "file_x.txt")
    )
    output <- kebabCase(path, rename = TRUE, recursive = TRUE)
    expect_null(output, expected)
    files <- sort(c(
        path,
        list.files(
            path = path,
            full.names = TRUE,
            recursive = TRUE,
            include.dirs = TRUE
        )
    ))
    expected <- c(
        path,
        file.path(
            path,
            c(
                "file-x.txt",
                "level-1",
                file.path("level-1", "file-x.txt"),
                file.path("level-1", "level-2"),
                file.path("level-1", "level-2", "file-x.txt")
            )
        )
    )
    expect_identical(files, expected)
})
