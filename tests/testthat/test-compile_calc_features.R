test_that("Basic functioning of compile_calc_features",{
    test_image <- matrix(runif(10201), 101)

    my_feat_funs <- compile_calc_features(
        filter_widths = c(3,5,7),
        shape_sizes = c(15, 31, 51),
        img_dim = dim(test_image))

    expect_named(my_feat_funs)
    expect_type(my_feat_funs[[1]], "closure")
})


test_that("compile_calc_features returns usable functions",{
    test_image <- matrix(runif(10201), 101)

    my_feat_funs <- compile_calc_features(
        filter_widths = c(3,5,7),
        shape_sizes = c(15, 31, 51),
        img_dim = dim(test_image))


    for (i in my_feat_funs) {
        ret <- i(test_image)
        expect_type(ret, "double")
        expect_equal(dim(ret), c(101, 101))
    }

    feat_tbl <- purrr::map_dfc(my_feat_funs, ~ as.numeric(.x(test_image)))

    display_filters(feat_tbl, c(101,101))
})



test_that("compile_calc_features returns usable functions on images",{
    test_image <- readImageBw(system.file(
        "extdata", "4T1-shNT-1.png",
        package = "clasifierrr"))

    my_feat_funs <- compile_calc_features(
        filter_widths = c(3,5,7),
        shape_sizes = c(15, 31, 51),
        img_dim = dim(test_image))


    for (i in my_feat_funs) {
        ret <- i(test_image)
        expect_type(ret, "double")
        expect_equal(dim(ret), dim(test_image))
    }

    feat_tbl <- purrr::map_dfc(my_feat_funs, ~ as.numeric(.x(test_image)))

    # display_filters(feat_tbl, dim(test_image))
    # display_filters(feat_tbl, dim(test_image), scale = TRUE)
})
