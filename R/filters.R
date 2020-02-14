
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


compile_sobel_filter <- function(width, dim_img) {
    sobel_kern <- calc_sobel_kern(width)
    fun_x <- prep_filter.filter(sobel_kern$x, dim_img)
    fun_y <- prep_filter.filter(sobel_kern$y, dim_img)

    .filter <- function(img) {
        mag_x <- fun_x(img)
        mag_y <- fun_y(img)

        ret <- sqrt( mag_x^2 + mag_y^2 )

        return(ret)
    }

    return(.filter)
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


compile_variance_filter <- function(width, dim_img) {
    sum_kernel <- EBImage::makeBrush(width, "disc")
    mean_kernell <- sum_kernel/sum(sum_kernel)
    mean_diff_kernell <- mean_kernell
    center <- (dim(mean_diff_kernell) %/% 2) + 1
    mean_diff_kernell[center[1], center[2]] <-
        mean_diff_kernell[center[1], center[2]] - 1

    fun_mean_diff <- prep_filter.filter(mean_diff_kernell, dim_img)
    fun_mean <- prep_filter.filter(mean_kernell, dim_img)

    # Hypothetically speaking, applying product of the filters in fft space would be eq.
    # to senquentially applying them.
    # I might be wrong ...

    #fun_sumsquares <- fun_sq_diff
    #environment(fun_sumsquares)$wf <- environment(fun_mean_img)$wf * environment(fun_sq_diff)$wf

    .filter <- function(img) {
        variances <- fun_mean(fun_mean_diff(img)^2)
        return(variances)
    }

    return(.filter)
}


variance_filter <- function(img, width,
                            img_fft = fftwtools::fftw2d(img)){
    # Generate Kernells
    sum_kernel <- EBImage::makeBrush(width, "disc")
    mean_kernell <- sum_kernel/sum(sum_kernel)
    mean_diff_kernell <- mean_kernell
    center <- (dim(mean_diff_kernell) %/% 2) + 1
    mean_diff_kernell[center[1], center[2]] <-
        mean_diff_kernell[center[1], center[2]] - 1

    # Apply kernell
    squares <- clasifierrr::filter2_circular(
        img, mean_diff_kernell,
        img_fft = fftwtools::fftw2d(img))^2

    variance <- clasifierrr::filter2_circular(
        squares, mean_kernell,
        img_fft = fftwtools::fftw2d(squares))

    return(variance)
}


dog_kernell <- function(sigma, ratio = 5) {
    smaller_size <- (2 * ceiling(3 * sigma) + 1)
    largest_size <- ratio * smaller_size


    small_gauss <- EBImage::makeBrush(
        size = largest_size,
        shape = "Gaussian",
        sigma = sigma)

    large_gauss <- EBImage::makeBrush(
        size = largest_size,
        shape = "Gaussian",
        sigma = sigma * ratio)

    combined_gauss <- large_gauss - small_gauss
    return(combined_gauss)
}

dog_filter <- function(img, width,
                       sigma = floor((width - 1) / 2)/3,
                       ratio = 5,
                       img_fft = fftwtools::fftw2d(img)){
    # > width
    # [1]  3  5  7  9 11
    # > floor((width - 1) / 2)/3
    # [1] 0.333 0.666 1.000 1.333 1.666
    #
    # > # for a ratio of 5 in sigma, the respective width would be
    # [1] 11 21 31 41 51
    #
    # Here width is the width of the smaller filter ...

    # Gaussian differences are one gaussien minus another less blurry (low sigma) one
    # Generate Kernells
    combined_gauss <- dog_kernell(sigma, ratio)

    # Apply kernell
    ret <- clasifierrr::filter2_circular(
        img, combined_gauss,
        img_fft = fftwtools::fftw2d(img))

    return(ret)
}

compile_dog_filter <- function(width, dim_img,
                               sigma = floor((width - 1) / 2)/3,
                               ratio = 5) {
    combined_gauss <- dog_kernell(sigma, ratio)
    fun_dog <- prep_filter.filter(combined_gauss, dim_img)

    .filter <- function(img) {
        dog_img <- fun_dog(img)
        return(dog_img)
    }

    return(.filter)
}

# TODO things to consider ...
# change the back-end in all functions to the pre-compilation.
# This would allow as well to automatically accept fourier transformed objects.