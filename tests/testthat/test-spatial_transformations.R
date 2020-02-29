test_that("compiling hough transform works on fft and real images", {
    myfile <- system.file("extdata", "tiny_4T1-shNT-1.png", package = "clasifierrr")
    myimg <- readImageBw(myfile)
    trans_fun_501 <- compile_circular_hough(101, sobel_width = 3, dim_img = dim(myimg))

    real_case <- trans_fun_501(myimg)
    fft_case <- trans_fun_501(fftwtools::fftw2d(myimg))

    expect_equal(real_case, fft_case)

    myfun <- compile_hough_circle_draw(
        width = 101, sobel_width = 3,
        dim_img = dim(myimg), tolerance = 11,
        pct_max = 0.95, blurr = 1/8)


    real_case <- myfun(myimg)
    fft_case <- myfun(fftwtools::fftw2d(myimg))

    expect_equal(real_case, fft_case)

})
