context("S4 : DataTable")

mcols(df) <- DataFrame(TEST = seq_len(ncol(df)))
metadata(df) <- list(TEST = "XXX")

with_parameters_test_that(
    "DataFrame", {
        x <- f(
            object = df,
            rownames = TRUE,
            colnames = TRUE,
            mcols = TRUE,
            metadata = TRUE
        )
        expect_s4_class(x, "DataFrame")
    },
    f = funs
)
