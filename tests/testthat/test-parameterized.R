context("parameterized")

## Don't attempt to test kebabCase here, it doesn't support names argument.
with_parameters_test_that(
    "Named", {
        object <- syntactic[["character_named"]]
        expect_identical(
            object = f(object, names = TRUE),
            expected = expected
        )
        expect_identical(
            object = names(f(object, names = FALSE)),
            expected = names(object)
        )
    },
    f = funs,
    expected = list(
        camelCase = c(
            itemA = "helloWorld",
            itemB = "helloWORLD"
        ),
        dottedCase = c(
            "Item.A" = "hello.world",
            "Item.B" = "HELLO.WORLD"
        ),
        snakeCase = c(
            "item_a" = "hello_world",
            "item_b" = "hello_world"
        ),
        upperCamelCase = c(
            ItemA = "HelloWorld",
            ItemB = "HELLOWORLD"
        )
    )
)

with_parameters_test_that(
    "Unnamed", {
        object <- syntactic[["character"]]
        expect_identical(
            object = f(object),
            expected = expected
        )
    },
    f = c(funs, kebabCase = kebabCase),
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
        ),
        kebabCase = c(
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
            NA
        )
    )
)

with_parameters_test_that(
    "camelCase of delimited numbers", {
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
