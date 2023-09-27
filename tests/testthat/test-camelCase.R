test_that("Strict mode", {
    expect_identical(
        object = camelCase(unnamed, strict = TRUE),
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
    expect_identical(
        object = camelCase(
            c(
                "cliUpdateRPackages",
                "externalIDs",
                "externalRNAs"
            ),
            strict = TRUE
        ),
        expected = c(
            "cliUpdateRPackages",
            "externalIds",
            "externalRnas"
        )
    )
})

test_that("Non-strict mode", {
    expect_identical(
        object = camelCase(unnamed, strict = FALSE),
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
        object = camelCase(named, names = TRUE, strict = TRUE),
        expected = c(
            itemA = "helloWorld",
            itemB = "helloWorld"
        )
    )
    expect_named(
        object = camelCase(named, names = FALSE),
        expected = names(named)
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
            "minusDox",
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
