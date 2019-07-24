context("S4 : SummarizedExperiment")

with_parameters_test_that(
    "SummarizedExperiment", {
        x <- f(
            object = rse,
            rownames = TRUE,
            colnames = TRUE,
            assayNames = TRUE,
            rowData = TRUE,
            colData = TRUE,
            metadata = TRUE
        )
        expect_s4_class(x, "SummarizedExperiment")
    },
    f = funs
)
