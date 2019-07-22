context("S4 : GRanges")

with_parameters_test_that(
    "GRanges", {
        x <- f(
            object = gr,
            names = TRUE,
            mcols = TRUE
        )
        expect_s4_class(x, "GRanges")
        expect_identical(
            object = colnames(mcols(x)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camelCase = c("geneID", "geneName"),
        dottedCase = c("gene.ID", "gene.Name"),
        snakeCase = c("gene_id", "gene_name"),
        upperCamelCase = c("GeneID", "GeneName")
    )
)
