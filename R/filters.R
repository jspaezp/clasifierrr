
# Internal function that calculates kernells used for image filtering,
# based on sobel-like edge detections, provided a width
# > calc_sobel_kern(3)
# $y
# [,1] [,2] [,3]
# [1,]   -1   -2   -1
# [2,]    0    0    0
# [3,]    1    2    1
#
# $x
# [,1] [,2] [,3]
# [1,]   -1    0    1
# [2,]   -2    0    2
# [3,]   -1    0    1
calc_sobel_kern <- function(width) {
    part <- c(seq(from = 1, to = (width + 1)/2, by = 1),
              seq(from = (width - 1)/2, to = 1, by = -1))

    m <- matrix(c(
        -part, rep(rep(0, length(part)), width -2),
        part),
        ncol = width,
        byrow = TRUE)

    return(list(y = m, x = t(m)))
}


sobel_filter <- function(img, width = 3,
                         img_fft = fftwtools::fftw2d(img)) {
    sobel_kern <- calc_sobel_kern(width)

    magx <- filter2_circular(
        img,
        sobel_kern$x,
        img_fft = img_fft)

    magy <- filter2_circular(
        img,
        sobel_kern$y,
        img_fft = img_fft)

    ret <- sqrt( magx^2 + magy^2 )
    return(ret)
}


variance_filter <- function(img, width,
                            img_fft = fftwtools::fftw2d(img)){
    sum_kernel <- EBImage::makeBrush(width, "disc")
    mean_kernell <- sum_kernel/sum(sum_kernel)

    mean_img <- filter2_circular(img, mean_kernell, img_fft = img_fft)
    squares <- (mean_img - img)^2
    variance <- filter2_circular(squares, sum_kernel, img_fft = img_fft)

    return(variance)
}