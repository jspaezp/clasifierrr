
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
        -part, rep(rep(0, length(part)), width - 2),
        part),
        ncol = width,
        byrow = TRUE)

    return(list(y = m, x = t(m)))
}

#' @describeIn sobel_filter pre-calculates parameters and returns a function
#' @export
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

#' Calculating sobel filters
#'
#' The sobel filter is used to detect edges in the images
#'
#' @param img Dimensions of the image
#' @param width width of the filter to be applied
#' @param img_fft optional argument to pass the pre-computed fourier tranform of
#'                the image from `fftwtools::fftw2d(img)`.
#' @param dim_img the dimensions of the image to be used later during filtering,
#'                only needed if pre-compiling the filter.
#'
#' @return
#' @export
#'
#' @family filters
#' @examples
#' test_image <- matrix(runif(81), 9)
#' sobel_filter(test_image, 3)
#'
#' my_sobel_filter <- compile_sobel_filter(3, dim(test_image))
#' my_sobel_filter(test_image)
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

#' @describeIn variance_filter pre-calculates parameters and returns a function
#' @export
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


#' Calculate variance filter of an image
#'
#' Ths filter accentuates areas of high textre
#'
#' @inheritParams sobel_filter
#'
#' @return
#' @export
#'
#' @family filters
#' @examples
#' test_image <- matrix(runif(81), 9)
#' variance_filter(test_image, 3)
#'
#' my_variance_filter <- compile_variance_filter(3, dim(test_image))
#' my_variance_filter(test_image)
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


dog_kernell <- function(largest_sigma, largest_width, ratio = 5) {
    # The sigma here would be the sigma of the smaller blur

    small_gauss <- EBImage::makeBrush(
        size = largest_width,
        shape = "Gaussian",
        sigma = largest_sigma / ratio)

    large_gauss <- EBImage::makeBrush(
        size = largest_width,
        shape = "Gaussian",
        sigma = largest_sigma)

    combined_gauss <- large_gauss - small_gauss
    return(combined_gauss)
}


#' Calculates the difference of gaussian filter
#'
#' This filter attempts to enhance the features in an image, it is based on
#' subtracting a blurry image from a less blurry one. The ratio of this
#' blurrines is measured by the ratio in their `sigma`.
#'
#' @inheritParams sobel_filter
#' @param ratio the ratio of the sigma between the blurrier and
#'              the less blurry images
#'
#' @return
#' @export
#'
#' @family filters
#' @examples
#' test_image <- matrix(runif(81), 9)
#' dog_filter(test_image, 3)
#'
#' my_dog_filter <- compile_dog_filter(3, dim(test_image))
#' my_dog_filter(test_image)
dog_filter <- function(img, width,
                       ratio = 5,
                       img_fft = fftwtools::fftw2d(img)){

    # This would be the largest sigma posible ...
    sigma <- ((width/2) - 1)/3

    # Gaussian differences are one gaussien minus another less blurry (low sigma) one
    # Generate Kernells
    combined_gauss <- dog_kernell(
        largest_sigma = sigma,
        largest_width = width,
        ratio)

    # Apply kernell
    ret <- clasifierrr::filter2_circular(
        img, combined_gauss,
        img_fft = fftwtools::fftw2d(img))

    return(ret)
}

#' @describeIn dog_filter pre-calculates parameters and returns a function
#' @export
compile_dog_filter <- function(width, dim_img,
                               ratio = 5) {
    # This would be the sigma for the largest ...
    sigma = ((width/2) - 1)/3

    combined_gauss <- dog_kernell(
        largest_sigma = sigma,
        largest_width = width,
        ratio)

    fun_dog <- prep_filter.filter(combined_gauss, dim_img)

    .filter <- function(img) {
        dog_img <- fun_dog(img)
        return(dog_img)
    }

    return(.filter)
}

donut_kernell <- function(width, tolerance) {
    tw <- tolerance + width
    if (tw %% 2 == 0) {
        tw <- tw + 1
    }
    mat <- matrix(rep(0, tw*tw),  nrow = tw)
    x <- EBImage::drawCircle(
        mat,
        radius = width %/% 2,
        x = (tw %/% 2),
        y = (tw %/% 2),
        col = 1)
    if (tolerance != 0) {
        x <- EBImage::dilate(x, EBImage::makeBrush(tolerance, "disc"))
    }
    kernell <- x/sum(x)

    return(kernell)
}


#' Calculates a circular hough transform
#'
#' This transform accentuates circular shapes
#'
#' @param img image to transform
#' @param width expected width of the circles
#' @param sobel_width width of the sobel filter that will be used to detect the edges
#'
#' @return image
#' @export
#' @family filters
#'
#' @examples
#' x = readImage(system.file('images', 'shapes.png', package='EBImage'))
#' hough <- circular_hough_transform(x, 51, 3, 5)
#' # display(normalize(hough))
#' @importFrom EBImage dilate makeBrush filter2
circular_hough_transform <- function(img, width, sobel_width, tolerance = 11) {
    kernell <- donut_kernell(width, tolerance)
    trans <- EBImage::medianFilter(img, 3)
    trans <- sobel_filter(trans, sobel_width)
    trans <- clean_image_border(trans, sobel_width)
    trans <- trans > 0.5
    trans <- EBImage::filter2(trans, kernell, boundary = 0)
    trans <- EBImage::filter2(trans, makeBrush(tolerance, "disc"), boundary = 0)
    return(trans)
}

#' @describeIn circular_hough_transform pre-calculates parameters and returns a function
#' @export
compile_circular_hough <- function(width, sobel_width, dim_img, tolerance = 11) {
    # This would be the sigma for the largest ...
    compiled_sobel <- compile_sobel_filter(sobel_width, dim_img)
    donut_kern <- donut_kernell(width, tolerance)
    fun_hough_circular <- prep_filter.filter(donut_kern, dim_img)

    .filter <- function(img) {
        if (is.complex(img)) {
            img <- Re(fftwtools::fftw2d(img, inverse = TRUE)/prod(dim(img)))
        }
        img <- EBImage::medianFilter(img, 3)
        img <- compiled_sobel(img)
        img <- clean_image_border(img, sobel_width)
        img <- img > 0.5
        hough_img <- fun_hough_circular(img)
        hough_img <- EBImage::filter2(hough_img, makeBrush(tolerance, "disc"), boundary = 0)

        return(hough_img)
    }

    return(.filter)
}


#' @describeIn hough_circle_draw pre-calculates parameters and returns a function
#' @export
compile_hough_circle_draw <- function(width, sobel_width, dim_img,
                                      tolerance = 11, pct_max = 0.95,
                                      blurr = 1/8){
    compiled_circ_hough <- compile_circular_hough(
        width = width, sobel_width = sobel_width,
        dim_img = dim_img, tolerance = tolerance)

    .filter <- function(img) {
        img <- compiled_circ_hough(img)
        near_max_vals <- img > pct_max * max(img)
        img[] <- as.numeric(near_max_vals)

        img <- EBImage::dilate(img, EBImage::makeBrush(width, "disc"))
        img <- EBImage::gblur(img, sigma = width * blurr)
        return(img)
    }

    return(.filter)
}


#' Calculates a circular hough transform and draws the predicted circles
#'
#' This transform accentuates circular shapes
#'
#' @inheritParams circular_hough_transform
#' @param pct_max the percentage of the maximum intensity that is considered to
#'                draw the circles (defaults to 0.95, anyting 95% of the highest
#'                intensity)
#' @param blurra numeric value indicating the ratio of the width to blurr,
#'               1 means have the blurr be the same as the circle width
#'
#' @return image
#' @export
#' @family filters
#'
#' @examples
#' x = readImage(system.file('images', 'shapes.png', package='EBImage'))
#' hough <- hough_circle_draw(x, 51, 3, 3)
#' # display(hough)
#' # display(generate_overlay(x, hough > 0.68))
#' hough <- hough_circle_draw(x, 71, 3, 3, 0.9)
#' # display(hough)
#' # display(generate_overlay(x, hough > 0.68))
#'
#' @importFrom EBImage gblur makeBrush
hough_circle_draw <- function(img, width, sobel_width,
                              tolerance, pct_max = 0.95,
                              blurr = 1/8) {
    hough_trans <- circular_hough_transform(
        img = img, width = width,
        sobel_width = sobel_width,
        tolerance = tolerance)

    near_max_vals <- hough_trans > pct_max * max(hough_trans)
    hough_trans[] <- as.numeric(near_max_vals)

    x <- EBImage::dilate(hough_trans, EBImage::makeBrush(width, "disc"))
    x <- EBImage::gblur(x, sigma = width * blurr)
    return(x)
}

# Experimental
hough_circles_max <- function(img,
                          radii = seq(from = 51, to = 601, by = 20),
                          sobel_width = 5,
                          quantile_draw = 0.9,
                          tolerance = 21) {
    # TECHNICALLY SPEAKING, this would be the diameter ...
    radii <- as.integer(radii)
    radii[radii %% 2 == 0] <- radii[radii %% 2 == 0] + 1

    # In This section, the transform could be saved in mem
    # if calculating an dditional transform later is too expensive
    maxpoint <- purrr::map_dbl(
        radii,
        ~ max(circular_hough_transform(img, .x, sobel_width = sobel_width,
                             tolerance = tolerance)))



    # Normalization is no longer required due to a change in the weighting
    # kernell (donut)
    norm_max <- maxpoint

    best_radius <- radii[which.max(norm_max)]
    message("Selecting ", best_radius, " as the best radius for the circles")

    hough_trans <- circular_hough_transform(
        img, best_radius,
        sobel_width, tolerance = tolerance)

    near_max_vals <- hough_trans > quantile_draw * max(hough_trans)
    hough_trans[] <- as.numeric(near_max_vals)

    x <- EBImage::dilate(hough_trans, EBImage::makeBrush(best_radius, "disc"))

    return(x)
}



# TODO things to consider ...
# change the back-end in all functions to the pre-compilation.
# This would allow as well to automatically accept fourier transformed objects.