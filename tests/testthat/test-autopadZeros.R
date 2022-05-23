test_that("integer", {
    ## Unmodified.
    expect_identical(
        object = autopadZeros(c(a = 1L, b = 10L, c = 100L)),
        expected = c(a = "001", b = "010", c = "100")
    )
    ## Left side.
    expect_identical(
        object = autopadZeros(c(a = 1L, b = 2L, c = 3L)),
        expected = c(a = "1", b = "2", c = "3")
    )
})

test_that("character", {
    ## Unmodified.
    expect_identical(
        object = autopadZeros(c(a = "A", b = "B", c = "C")),
        expected = c(a = "A", b = "B", c = "C")
    )
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

test_that("Partial padding", {
    expect_error(
        object = autopadZeros(c("1", "10", "X")),
        regexp = "Partial padding match detected."
    )
    object <- c(
        "dmso-1", "dmso-2", "dmso-3",
        "drug1-300nm-1", "drug1-300nm-2", "drug1-300nm-3",
        "drug1-drug3-1", "drug1-drug3-2", "drug1-drug3-3",
        "drug2-100nm-1", "drug2-100nm-2", "drug2-100nm-3",
        "drug2-drug1-1", "drug2-drug1-2", "drug2-drug1-3",
        "drug2-drug3-1", "drug2-drug3-2", "drug2-drug3-3",
        "drug3-300nm-1", "drug3-300nm-2", "drug3-300nm-3",
        "triple-combo-1", "triple-combo-2", "triple-combo-3"
    )
    expect_identical(autopadZeros(object), object)
})
