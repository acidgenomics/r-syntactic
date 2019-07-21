context("makeDimnames")

test_that("makeDimnames", {
    mat <- matrix(
        data = seq_len(4L),
        nrow = 2L,
        dimnames = list(
            c("1-a", "1-a"),
            c("2-b", "2-b")
        )
    )
    expect_identical(
        object = dimnames(makeDimnames(mat)),
        expected = list(
            c("X1_a", "X1_a_1"),
            c("X2_b", "X2_b_1")
        )
    )
})
