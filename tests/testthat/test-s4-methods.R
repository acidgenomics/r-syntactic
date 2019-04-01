context("S4 methods")

data(gr, mn, package = "acidtest", envir = environment())

funs <- list(
    camel = camel,
    dotted = dotted,
    snake = snake,
    upperCamel = upperCamel
)



# atomic =======================================================================
with_parameters_test_that(
    "makeNames : atomic : unnamed", {
        x <- 1L
        expect_identical(f(x), x)
    },
    f = funs
)

with_parameters_test_that(
    "makeNames : atomic : named", {
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



# character ====================================================================
with_parameters_test_that(
    "makeNames : character", {
        object <- mn[["character"]]
        expect_identical(f(object), expected)
    },
    f = funs,
    expected = list(
        camel = c(
            "helloWorld",
            "helloWORLD",
            "rnaiClones",
            "nCount",
            "tx2gene",
            "tx2GeneID",
            "g2mScore",
            "worfdbHTMLRemap",
            "mazdaRX4",
            "percentGC",
            "x5prime",
            "x5x3Bias",
            "x123",
            NA
        ),
        dotted = c(
            "hello.world",
            "HELLO.WORLD",
            "RNAI.clones",
            "n.Count",
            "tx2gene",
            "TX2.Gene.ID",
            "G2M.Score",
            "worfdb.HTML.Remap",
            "Mazda.RX4",
            "percent.GC",
            "X5prime",
            "X5.3.bias",
            "X123",
            NA
        ),
        snake = c(
            "hello_world",
            "hello_world",
            "rnai_clones",
            "n_count",
            "tx2gene",
            "tx2_gene_id",
            "g2m_score",
            "worfdb_html_remap",
            "mazda_rx4",
            "percent_gc",
            "x5prime",
            "x5_3_bias",
            "x123",
            NA
        ),
        upperCamel = c(
            "HelloWorld",
            "HELLOWORLD",  # improve this?
            "RNAIClones",
            "NCount",
            "Tx2gene",
            "TX2GeneID",
            "G2MScore",
            "WorfdbHTMLRemap",
            "MazdaRX4",
            "PercentGC",
            "X5prime",
            "X5X3Bias",
            "X123",
            NA
        )
    )
)

with_parameters_test_that(
    "makeNames : character (named)", {
        object <- mn[["namedCharacter"]]
        expect_identical(f(object), expected)
    },
    f = funs,
    expected = list(
        camel = c(itemA = "helloWorld", itemB = "helloWORLD"),
        dotted = c("Item.A" = "hello.world", "Item.B" = "HELLO.WORLD"),
        snake = c("item_a" = "hello_world", "item_b" = "hello_world"),
        upperCamel = c(ItemA = "HelloWorld", ItemB = "HELLOWORLD")
    )
)

with_parameters_test_that(
    "makeNames : character : Delimited numbers", {
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
    f = list(camel, camel, upperCamel),
    strict = list(FALSE, TRUE, TRUE),
    expected = list(
        camel_normal = c(
            "x1000000",
            "x0x01",
            "x2018x01x01",
            "res0x1"
        ),
        camel_strict = c(
            "x1000000",
            "x0x01",
            "x2018x01x01",
            "res0x1"
        ),
        upperCamel_strict = c(
            "X1000000",
            "X0X01",
            "X2018X01X01",
            "Res0X1"
        )
    )
)



# factor =======================================================================
with_parameters_test_that(
    "makeNames : factor", {
        object <- mn[["factor"]]
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
        camel = c("group1", "group2"),
        dotted = c("group.1", "group.2"),
        snake = c("group_1", "group_2"),
        upperCamel = c("Group1", "Group2")
    ),
    names = list(
        camel = c("sample1", "sample2", "sample3", "sample4"),
        dotted = c("sample.1", "sample.2", "sample.3", "sample.4"),
        snake = c("sample_1", "sample_2", "sample_3", "sample_4"),
        upperCamel = c("Sample1", "Sample2", "Sample3", "Sample4")
    )
)



# matrix / data.frame ==========================================================
with_parameters_test_that(
    "makeNames : matrix", {
        object <- mn[["matrix"]][1L:3L, 1L:3L]
        expect_identical(
            object = dimnames(f(object, rownames = TRUE, colnames = TRUE)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camel = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urbanPop")
        ),
        dotted = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "Urban.Pop")
        ),
        snake = list(
            c("alabama", "alaska", "arizona"),
            c("murder", "assault", "urban_pop")
        ),
        upperCamel = list(
            c("Alabama", "Alaska", "Arizona"),
            c("Murder", "Assault", "UrbanPop")
        )
    )
)



# list =========================================================================
with_parameters_test_that(
    "makeNames : list", {
        object <- mn[["list"]]
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



# GRanges ======================================================================
test_that("camel : GRanges", {
    # gr object is already camel formatted.
    expect <- list(
        camel = c("geneID", "geneName"),
        dotted = c("gene.ID", "gene.Name"),
        snake = c("gene_id", "gene_name"),
        upperCamel = c("GeneID", "GeneName"))
    lapply(seq_along(funs), function(i) {
        expect_identical(
            colnames(mcols(funs[[i]](gr))),
            expect[[i]]
        )
    })
})



# camel-specific ===============================================================
with_parameters_test_that(
    "camel : Strict mode", {
        x <- mn[["character"]]
        expect_identical(f(x, strict = TRUE), expected)
    },
    f = list(camel, upperCamel),
    expected = list(
        camel = c(
            "helloWorld",
            "helloWorld",
            "rnaiClones",
            "nCount",
            "tx2gene",
            "tx2GeneId",
            "g2mScore",
            "worfdbHtmlRemap",
            "mazdaRx4",
            "percentGc",
            "x5prime",
            "x5x3Bias",
            "x123",
            NA
        ),
        upperCamel = c(
            "HelloWorld",
            "HelloWorld",
            "RnaiClones",
            "NCount",
            "Tx2gene",
            "Tx2GeneId",
            "G2mScore",
            "WorfdbHtmlRemap",
            "MazdaRx4",
            "PercentGc",
            "X5prime",
            "X5X3Bias",
            "X123",
            NA
        )
    )
)
