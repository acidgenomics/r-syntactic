context("autopadZeros")

test_that("integer", {
    expect_identical(
        object = autopadZeros(c(a = 1L, b = 10L, c = 100L)),
        expected = c(a = "001", b = "010", c = "100")
    )
})

test_that("character", {
    expect_identical(
        object = autopadZeros(c(a = "A1", b = "B10")),
        expected = c(a = "A01", b = "B10")
    )
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
