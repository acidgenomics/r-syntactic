context("character")

with_parameters_test_that(
    "Unnamed", {
        expect_identical(
            object = f(syntactic[["character"]]),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camelCase = c(
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
            "x123",
            NA
        ),
        dottedCase = c(
            "percent.GC",
            "X10um",
            "X5.3.bias",
            "X5prime",
            "G2M.Score",
            "hello.world",
            "HELLO.WORLD",
            "Mazda.RX4",
            "n.Count",
            "RNAI.clones",
            "tx2gene",
            "TX2.Gene.ID",
            "worfdb.HTML.Remap",
            "X123",
            NA
        ),
        snakeCase = c(
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
            "x123",
            NA
        ),
        upperCamelCase = c(
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
            "X123",
            NA
        )
    )
)

with_parameters_test_that(
    "Named", {
        expect_identical(
            object = f(syntactic[["character_named"]]),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camelCase = c(itemA = "helloWorld", itemB = "helloWORLD"),
        dottedCase = c("Item.A" = "hello.world", "Item.B" = "HELLO.WORLD"),
        snakeCase = c("item_a" = "hello_world", "item_b" = "hello_world"),
        upperCamelCase = c(ItemA = "HelloWorld", ItemB = "HELLOWORLD")
    )
)

with_parameters_test_that(
    "Delimited numbers", {
        object <- c(
            "1,000,000",
            "0.01",
            "2018-01-01",
            "res.0.1"
        )
        expect_identical(
            object = f(object, strict = strict),
            expected = expected
        )
    },
    f = list(
        camelCase,
        camelCase,
        upperCamelCase
    ),
    strict = list(
        FALSE,
        TRUE,
        TRUE
    ),
    expected = list(
        camelCase_normal = c(
            "x1000000",
            "x0x01",
            "x2018x01x01",
            "res0x1"
        ),
        camelCase_strict = c(
            "x1000000",
            "x0x01",
            "x2018x01x01",
            "res0x1"
        ),
        upperCamelCase_strict = c(
            "X1000000",
            "X0X01",
            "X2018X01X01",
            "Res0X1"
        )
    )
)
