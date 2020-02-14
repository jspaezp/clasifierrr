test_that("Variance filter calculates variance correctly", {
    matrix_size <- 500
    filter_size <- 201
    test_mat <- matrix(rnorm(matrix_size^2, 0, 2), matrix_size)
    expect_equal(mean(variance_filter(test_mat, filter_size)), 2^2, tolerance = 200)

    test_mat <- matrix(rnorm(matrix_size^2, 0, 200), matrix_size)
    expect_equal(mean(variance_filter(test_mat, filter_size)), 200^2, tolerance = 2)


})


test_that("Compiled version of the variance calculation works", {
    matrix_size <- 500
    filter_size <- 201
    test_mat <- matrix(rnorm(matrix_size^2, 0, 2), matrix_size)

    compiled_var <- compile_variance_filter(
        width = filter_size,
        dim_img = dim(test_mat))

    expect_equal(compiled_var(test_mat), variance_filter(test_mat, filter_size))

    test_mat <- matrix(rnorm(matrix_size^2, 0, 200), matrix_size)

    expect_equal(compiled_var(test_mat), variance_filter(test_mat, filter_size))


})


test_that("Precompiled sobel filter works consistently with the traditional version",{
    img <- matrix(rnorm(matrix_size^2, 0, 2), matrix_size)

    fft_img <- fftwtools::fftw2d(img)
    trad_sobel <- sobel_filter(img, 3)
    compiled_sobel <- compile_sobel_filter(3, dim(img))
    precompiled_sobel <- compiled_sobel(img)
    expect_equal(trad_sobel, precompiled_sobel)

    trad_sobel <- sobel_filter(img, 5)
    compiled_sobel <- compile_sobel_filter(5, dim(img))
    precompiled_sobel <- compiled_sobel(img)

    expect_equal(trad_sobel, precompiled_sobel)
    expect_equal(compiled_sobel(fft_img), compiled_sobel(img))
})
