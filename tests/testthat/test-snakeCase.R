context("snakeCase")

test_that("Plus minus handling", {
    expect_identical(
        object = snakeCase(pm),
        expected = c(
            "x100_percent",
            "plus_slash_minus",
            "a_plus_slash_minus_b",
            "dox_minus",
            "dox_plus",
            "x_dox",
            "plus_dox",
            "slash",
            "x"
        )
    )
})
