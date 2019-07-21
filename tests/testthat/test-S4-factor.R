context("S4 : factor")

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
