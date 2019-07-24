context("S4 : list")

with_parameters_test_that(
    "list", {
        object <- syntactic[["list"]]
        expect_identical(
            object = names(f(object)),
            expected = expected
        )
    },
    f = funs,
    expected = list(
        camel = c("itemA", "itemB"),
        dotted = c("Item.A", "Item.B"),
        snake = c("item_a", "item_b"),
        upperCamel = c("ItemA", "ItemB")
    )
)
