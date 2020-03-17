test_that("converting number to odd works", {
    odds <- make_odd(1:200)
    expect_true(all(odds %% 2 == 1))
})

test_that("converting doubles to odd works", {
    odds <- make_odd(1:200 + 0.1)
    expect_type(odds, "integer")
    expect_true(all(odds %% 2 == 1))
})
