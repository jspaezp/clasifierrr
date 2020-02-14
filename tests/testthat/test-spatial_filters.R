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
    matrix_size <- 500
    filter_size <- 201

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


test_that("DOG filters gets calculated correctly on matrix imput", {
    matrix_size <- 500
    filter_size <- 3

    img <- matrix(abs(rnorm(matrix_size^2, 0, 2)), matrix_size)

    fft_img <- fftwtools::fftw2d(img)
    trad_dog <- dog_filter(img, width = filter_size, ratio = 5)
    compiled_dog <- compile_dog_filter(width = filter_size, dim_img = dim(img), ratio = 5)
    precompiled_dog <- compiled_dog(img)
    expect_equal(trad_dog, precompiled_dog)

    trad_dog <- dog_filter(img, filter_size)
    compiled_dog <- compile_dog_filter(filter_size, dim(img))
    precompiled_dog <- compiled_dog(img)

    expect_equal(trad_dog, precompiled_dog)
    expect_equal(compiled_dog(fft_img), compiled_dog(img))

    filter_size <- 11

    trad_dog <- dog_filter(img, width = filter_size, ratio = 5)
    compiled_dog <- compile_dog_filter(width = filter_size, dim_img = dim(img), ratio = 5)
    precompiled_dog <- compiled_dog(img)
    expect_equal(trad_dog, precompiled_dog)

    trad_dog <- dog_filter(img, filter_size)
    compiled_dog <- compile_dog_filter(filter_size, dim(img))
    precompiled_dog <- compiled_dog(img)

    expect_equal(trad_dog, precompiled_dog)
    expect_equal(compiled_dog(fft_img), compiled_dog(img))


})


test_that("DOG filters gets calculated correctly on image imput", {
    img <- readImageBw(system.file(
        "extdata", "4T1-shNT-1.png",
        package = "clasifierrr"))

    fft_img <- fftwtools::fftw2d(img)
    trad_dog <- dog_filter(img, width = 3)
    compiled_dog <- compile_dog_filter(width = 3, dim_img = dim(img), 5)
    precompiled_dog <- compiled_dog(img)
    # expect_equal(trad_dog, precompiled_dog)

    # EBImage::display(precompiled_dog, method = "raster")
    trad_dog <- dog_filter(img, 5)
    compiled_dog <- compile_dog_filter(5, dim(img))
    precompiled_dog <- compiled_dog(img)

    # expect_equal(trad_dog, precompiled_dog)
    # EBImage::display(trad_dog, method = "raster")
    # EBImage::display(precompiled_dog, method = "raster")
    expect_equal(compiled_dog(fft_img), compiled_dog(img))

})
