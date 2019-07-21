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
