context("upperCamelCase")

test_that("Unnamed character", {
    expect_identical(
        object = upperCamelCase(unnamed),
        expected = c(
            "PercentGC",
            "X10um",
            "X5X3Bias",
            "X5prime",
            "G2MScore",
            "HelloWorld",
            "HELLOWORLD",
            "MazdaRX4",
            "NCount",
            "RNAIClones",
            "Tx2gene",
            "TX2GeneID",
            "WorfdbHTMLRemap",
            "X123"
        )
    )
})

test_that("Named character", {
    expect_identical(
        object = upperCamelCase(named, names = TRUE),
        expected = c(
            ItemA = "HelloWorld",
            ItemB = "HELLOWORLD"
        )
    )
    expect_identical(
        object = names(camelCase(named, names = FALSE)),
        expected = names(named)
    )
})

test_that("Strict mode", {
    expect_identical(
        object = upperCamelCase(vec, strict = TRUE),
        expected = c(
            "PercentGc",
            "X10um",
            "X5X3Bias",
            "X5prime",
            "G2mScore",
            "HelloWorld",
            "HelloWorld",
            "MazdaRx4",
            "NCount",
            "RnaiClones",
            "Tx2gene",
            "Tx2GeneId",
            "WorfdbHtmlRemap",
            "X123"
        )
    )
})

test_that("Disable X prefix", {
    object <- c("1" = "1 foo bar")
    expect_identical(
        object = upperCamelCase(object),
        expected = c("X1" = "X1FooBar")
    )
    expect_identical(
        object = upperCamelCase(object, prefix = FALSE),
        expected = c("X1" = "1FooBar")
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
    output <- upperCamelCase(input, rename = TRUE, prefix = FALSE)
    expect_identical(
        object = output,
        expected = file.path(
            topdir,
            c(
                "1SampleA.fastq.gz",
                "HelloWorld.txt",
                "AaaBbb",
                "CccDdd"
            )
        )
    )
    unlink(topdir, recursive = TRUE)
})
