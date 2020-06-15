context("autopadZeros")

test_that("integer", {
    expect_identical(
        object = autopadZeros(c(a = 1L, b = 10L, c = 100L)),
        expected = c(a = "001", b = "010", c = "100")
    )
    expect_identical(
        object = autopadZeros(c(a = 1L, b = 2L, c = 3L)),
        expected = c(a = "1", b = "2", c = "3")
    )
})

test_that("character", {
    ## Left side.
    expect_identical(
        object = autopadZeros(c(a = "1-EV", b = "10-EV", c = "100-EV")),
        expected = c(a = "001-EV", b = "010-EV", c = "100-EV")
    )
    ## Right side.
    expect_identical(
        object = autopadZeros(c(a = "A1", b = "B10", c = "C100")),
        expected = c(a = "A001", b = "B010", c = "C100")
    )
})

test_that("Partial padding error", {
    expect_error(
        object = autopadZeros(c("1", "10", "X")),
        regexp = "Partial padding match detected."
    )
})
