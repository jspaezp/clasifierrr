
#' Calculate filter features
#'
#' Calculates features for each pixel based on sobel filters, gaussian and
#' difference of gaussians, adds as well the position in the image
#'
#' @param img
#' @param filter_widths a numeric vector of odd numbers to be used as the
#'     width of the feature filters
#'
#' @return data.frame
#' @export
#'
#' @examples
#' @importFrom purrr map map_dfc
#' @importFrom furrr fiture_map future_map_dfc
#' @importFrom EBImage Image makeBrush filter2
calc_features <- function(img, filter_widths = c(3,5,11,23),
                          verbose = FALSE){
    start_time <- Sys.time()
    message("Starting to calculate features for image of width ", ncol(img),
            " and height ", nrow(img), "\n")

    message("Filters of size: {", paste0(filter_widths, collapse = ","), "}\n")

    img_fft <- fftwtools::fftw2d(img)

    filter_widths <- sort(filter_widths)

    g_filtered <- furrr::future_map_dfc(
        filter_widths,
        ~ as.numeric(
            filter2_circular(
                img,
                filter = EBImage::makeBrush(
                    size = 2 * ceiling(3 * .x) + 1,
                    shape = "Gaussian",
                    sigma = .x),
                img_fft = img_fft)))
    names(g_filtered) <- paste0("gauss_filt_", filter_widths)

    # TODO microbenchmark if its faster to call by index or argument
    # TODO consider calculating combinations instead of sequential subtractions
    # combn(1:3, 2, function(...) paste(..., collapse = ", "))
    # Gaussian differences are one gaussien minus another less blurry one
    g_diff <- furrr::future_map2_dfc(
        # First image will have lower sigma, aka less blurry
        g_filtered[, -length(g_filtered)], g_filtered[,-1],
        ~ 2*(as.numeric(.y) - as.numeric(.x)))

    names(g_diff) <- paste0("gauss_diff_", filter_widths[-length(g_filtered)])

    sobel_funs <- purrr::map(
        filter_widths,
        function(.x, img_fft) {
            function(img) {
                sobel_filter(img, width = .x, img_fft = img_fft)
            }}, img_fft)

    names(sobel_funs) <- paste0("sobel_filt_", filter_widths)

    v_funs <- purrr::map(
        filter_widths,
        function(.x, img_fft) {
            function(img) {
                variance_filter(img, width = .x, img_fft = img_fft)
            }}, img_fft)

    names(v_funs) <-  paste0("var_filt_", filter_widths)

    bound_flat <- furrr::future_map_dfc(
        c(v_funs, sobel_funs),
        ~ as.numeric(.x(img)),
        .options = furrr::future_options(
            packages = "clasifierrr",
            globals = c("img")))

    bound_flat <- do.call(cbind, list(g_filtered, g_diff, bound_flat))

    time_taken <- Sys.time() - start_time

    message("\nTook ", format(as.numeric(time_taken), digits = 2),
            " ", attr(time_taken, "units"),
            " to calculate the ", ncol(bound_flat),
            " features for ", nrow(bound_flat), " pixels\n")

    return(bound_flat)
}