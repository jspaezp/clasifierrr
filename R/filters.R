
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


sobel_filter <- function(img, width = 3, sobel_kern = NULL) {
    if (is.null(sobel_kern)) {
        sobel_kern <- calc_sobel_kern(width)
    }

    magx <- EBImage::filter2(
        img,
        sobel_kern$x,
        boundary = 'replicate')

    magy <- EBImage::filter2(
        img,
        sobel_kern$y,
        boundary = 'replicate')

    ret <- sqrt( magx^2 + magy^2 )
    return(ret)
}


variance_filter <- function(img, width){
    sum_kernel <- EBImage::makeBrush(width, "disc")
    mean_kernell <- sum_kernel/sum(sum_kernel)

    mean_img <- EBImage::filter2(img, mean_kernell, boundary = "replicate")
    squares <- (mean_img - img)^2


    variance <- EBImage::filter2(squares, sum_kernel, boundary = "replicate")
    return(variance)
}