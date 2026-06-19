test_that("Unnamed character", {
    expect_identical(
        object = snakeCase(unnamed),
        expected = c(
            "percent_gc",
            "x10um",
            "x5_3_bias",
            "x5prime",
            "g2m_score",
            "hello_world",
            "hello_world",
            "mazda_rx4",
            "n_count",
            "rnai_clones",
            "tx2gene",
            "tx2_gene_id",
            "worfdb_html_remap",
            "x123"
        )
    )
    expect_identical(
        object = snakeCase(c(
            "cliUpdateRPackages",
            "externalIDs",
            "externalRNAs"
        )),
        expected = c(
            "cli_update_r_packages",
            "external_ids",
            "external_rnas"
        )
    )
})

test_that("Named character", {
    expect_identical(
        object = snakeCase(named, names = TRUE),
        expected = c(
            "item_a" = "hello_world",
            "item_b" = "hello_world"
        )
    )
    expect_named(
        object = camelCase(named, names = FALSE),
        expected = names(named)
    )
})

test_that("Plus minus handling", {
    expect_identical(
        object = snakeCase(pm),
        expected = c(
            "x100_percent",
            "plus_slash_minus",
            "a_plus_slash_minus_b",
            "dox_minus",
            "dox_plus",
            "minus_dox",
            "plus_dox",
            "slash",
            "x"
        )
    )
})

test_that("RNA type sanitization", {
    expect_identical(
        object = snakeCase(c("miRNA", "ncRNA", "piRNA", "rRNA", "mRNA")),
        expected = c("mirna", "ncrna", "pirna", "rrna", "mrna")
    )
    expect_identical(
        object = snakeCase(c("miRNA_samples", "rRNA_depletion")),
        expected = c("mirna_samples", "rrna_depletion")
    )
})

test_that("Truncation fix: acronym before trailing content", {
    expect_identical(
        object = snakeCase("ABCd5test"),
        expected = "ab_cd5test"
    )
})
