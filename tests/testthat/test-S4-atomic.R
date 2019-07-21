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
