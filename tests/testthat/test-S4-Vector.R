context("S4 : Vector")

object <- as(df, "Vector")
names(object) <- toupper(names(object))
mcols(object) <- DataFrame(TEST = seq_len(length(object)))
metadata(object) <- list(TEST = "XXX")

with_parameters_test_that(
    "SimpleList", {
        x <- f(
            object = object,
            names = TRUE,
            mcols = TRUE,
            metadata = TRUE
        )
        expect_s4_class(x, "SimpleList")
    },
    f = funs
)
