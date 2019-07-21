context("atomic")

with_parameters_test_that(
    "Unnamed", {
        object <- 1L
        expect_identical(f(object), object)
    },
    f = funs
)

with_parameters_test_that(
    "Named", {
        object <- c("hello.world" = 1L)
        expect_identical(names(f(object)), expected)
    },
    f = funs,
    expected = c(
        "helloWorld",
        "hello.world",
        "hello_world",
        "HelloWorld"
    )
)



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



context("factor")

with_parameters_test_that(
    "factor", {
        object <- syntactic[["factor"]]
        expect_identical(
            object = levels(f(object)),
            expected = levels
        )
        expect_identical(
            object = names(f(object)),
            expected = names
        )
    },
    f = funs,
    levels = list(
        camelCase = c("group1", "group2"),
        dottedCase = c("group.1", "group.2"),
        snakeCase = c("group_1", "group_2"),
        upperCamelCase = c("Group1", "Group2")
    ),
    names = list(
        camelCase = c("sample1", "sample2", "sample3", "sample4"),
        dottedCase = c("sample.1", "sample.2", "sample.3", "sample.4"),
        snakeCase = c("sample_1", "sample_2", "sample_3", "sample_4"),
        upperCamelCase = c("Sample1", "Sample2", "Sample3", "Sample4")
    )
)



context("matrix")

with_parameters_test_that(
    "matrix", {
        object <- syntactic[["matrix"]][1L:3L, 1L:3L]
        expect_identical(
            object = dimnames(f(object, rownames = TRUE, colnames = TRUE)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camelCase = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urbanPop")
        ),
        dottedCase = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "Urban.Pop")
        ),
        snakeCase = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urban_pop")
        ),
        upperCamelCase = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "UrbanPop")
        )
    )
)



context("list")

with_parameters_test_that(
    "list", {
        object <- syntactic[["list"]]
        expect_identical(
            object = names(f(object)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camel = c("itemA", "itemB"),
        dotted = c("Item.A", "Item.B"),
        snake = c("item_a", "item_b"),
        upperCamel = c("ItemA", "ItemB")
    )
)



context("GRanges")

with_parameters_test_that(
    "GRanges", {
        x <- f(gr)
        expect_identical(
            object = colnames(mcols(x)),
            expected = expected
        )
    },
    f = funs,
    ## gr object is already camel formatted.
    expected = list(
        camelCase = c("geneID", "geneName"),
        dottedCase = c("gene.ID", "gene.Name"),
        snakeCase = c("gene_id", "gene_name"),
        upperCamelCase = c("GeneID", "GeneName")
    )
)
