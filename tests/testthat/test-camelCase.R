context("camelCase")

test_that("Unnamed character", {
    expect_identical(
        object = camelCase(unnamed),
        expected = c(
            "percentGC",
            "x10um",
            "x5x3Bias",
            "x5prime",
            "g2mScore",
            "helloWorld",
            "helloWORLD",
            "mazdaRX4",
            "nCount",
            "rnaiClones",
            "tx2gene",
            "tx2GeneID",
            "worfdbHTMLRemap",
            "x123"
        )
    )
})

test_that("Named character", {
    expect_identical(
        object = camelCase(named, names = TRUE),
        expected = c(
            itemA = "helloWorld",
            itemB = "helloWORLD"
        )
    )
    expect_identical(
        object = names(camelCase(named, names = FALSE)),
        expected = names(named)
    )
})

test_that("Strict mode", {
    expect_identical(
        object = camelCase(vec, strict = TRUE),
        expected = c(
            "percentGc",
            "x10um",
            "x5x3Bias",
            "x5prime",
            "g2mScore",
            "helloWorld",
            "helloWorld",
            "mazdaRx4",
            "nCount",
            "rnaiClones",
            "tx2gene",
            "tx2GeneId",
            "worfdbHtmlRemap",
            "x123"
        )
    )
})

test_that("Delimited numbers", {
    expect_identical(
        object = camelCase(dn),
        expected = c(
            "x1000000",
            "x0x01",
            "x2018x01x01",
            "res0x1"
        )
    )
})

test_that("Plus minus handling", {
    expect_identical(
        object = camelCase(pm),
        expected = c(
            "x100Percent",
            "plusSlashMinus",
            "aPlusSlashMinusB",
            "doxMinus",
            "doxPlus",
            "xDox",
            "plusDox",
            "slash",
            "x"
        )
    )
})

test_that("Disable X prefix", {
    object <- c("1" = "1 foo bar")
    expect_identical(
        object = camelCase(object),
        expected = c("x1" = "x1FooBar")
    )
    expect_identical(
        object = camelCase(object, prefix = FALSE),
        expected = c("x1" = "1FooBar")
    )
})

test_that("Rename mode", {
    topdir <- "XXX"
    unlink(topdir, recursive = TRUE)
    topdir <- initDir(topdir)
    dirs <- file.path(topdir, c("aaa-bbb", "ccc-ddd"))
    files <- file.path(topdir, c("1-sample-A.fastq.gz", "hello-world.txt"))
    input <- c(files, dirs)
    lapply(dirs, dir.create, recursive = TRUE)
    file.create(files)
    output <- camelCase(input, rename = TRUE)
    expected <- file.path(
        topdir,
        c(
            "1SampleA.fastq.gz",
            "helloWorld.txt",
            "aaaBbb",
            "cccDdd"
        )
    )
    expect_identical(object = output, expected = expected)
    unlink(topdir, recursive = TRUE)
})

test_that("X handling in prefix mode", {
    expect_identical(
        object = camelCase(
            object = c(
                "Xenobiotic",
                "xenobiotic",
                "XX123",
                "X123",
                "xx123",
                "x123",
                "123"
            ),
            prefix = FALSE
        ),
        expected = c(
            "xenobiotic",
            "xenobiotic",
            "xx123",
            "123",
            "xx123",
            "123",
            "123"
        )
    )
})
