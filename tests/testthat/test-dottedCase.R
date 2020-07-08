context("dottedCase")

test_that("Unnamed character", {
    expect_identical(
        object = dottedCase(unnamed),
        expected = c(
            "percent.gc",
            "x10um",
            "x5.3.bias",
            "x5prime",
            "g2m.score",
            "hello.world",
            "hello.world",
            "mazda.rx4",
            "n.count",
            "rnai.clones",
            "tx2gene",
            "tx2.gene.id",
            "worfdb.html.remap",
            "x123"
        )
    )
})

test_that("Named character", {
    expect_identical(
        object = dottedCase(named, names = TRUE),
        expected = c(
            "item.a" = "hello.world",
            "item.b" = "hello.world"
        )
    )
    expect_identical(
        object = names(camelCase(named, names = FALSE)),
        expected = names(named)
    )
})

test_that("X handling in prefix mode", {
    expect_identical(
        object = dottedCase(
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

test_that("Ampersand to 'and'", {
    expect_identical(
        object = dottedCase("here&there", smart = TRUE),
        expected = "here.and.there"
    )
    expect_identical(
        object = dottedCase("here&there", smart = FALSE),
        expected = "here.there"
    )
})

test_that("Accented characters", {
    object <- c(
        "bière",
        "encyclopædia",
        "étude",
        "qué tal"
    )
    expect_identical(
        object = dottedCase(object, smart = TRUE),
        expected = c(
            "biere",
            "encyclopaedia",
            "etude",
            "que.tal"
        )
    )
    ## > expect_identical(
    ## >     object = dottedCase(object, smart = FALSE),
    ## >     expected = c(
    ## >         "bière",
    ## >         "encyclopædia",
    ## >         "étude",
    ## >         "qué.tal"
    ## >     )
    ## > )
})
