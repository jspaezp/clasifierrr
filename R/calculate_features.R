
#' Calculate filter features
#'
#' Calculates features for each pixel based on sobel filters, gaussian and
#' difference of gaussians, adds as well the position in the image
#'
#' @param img an imput image or matrix
#' @param filter_widths a numeric vector of odd numbers to be used as the
#'     width of the feature filters
#' @param verbose wether to display progress messages
#'
#' @return data.frame
#' @export
#'
#' @examples
#' test_image <- matrix(runif(10201), 101)
#' feature_df <- calc_features(test_image, c(3,5))
#' head(feature_df)
#'
#' feature_funs <- compile_calc_features(filter_widths = c(3,5), dim(test_image))
#' feature_funs[[1]](test_image)
#'
#' feature_df2 <- purrr::map_dfc(feature_funs, ~ as.numeric(.x(test_image)))
#' head(feature_df2)
#'
#' @importFrom purrr map map_dfc
#' @importFrom furrr future_map future_map_dfc
#' @importFrom EBImage Image makeBrush filter2
calc_features <- function(img, filter_widths = c(3,5,11,23),
                          verbose = FALSE){
    start_time <- Sys.time()
    if (verbose) message(
        "Starting to calculate features for image of width ", ncol(img),
        " and height ", nrow(img), "\n")

    if (verbose) message(
        "Filters of size: {", paste0(filter_widths, collapse = ","), "}\n")

    img_fft <- fftwtools::fftw2d(img)
    filter_widths <- sort(filter_widths)

    feature_functions <- compile_calc_features(filter_widths = filter_widths, dim(img))

    bound_flat <- furrr::future_map_dfc(
        feature_functions,
        ~ as.numeric(.x(img_fft)),
        .options = furrr::future_options(
            packages = "clasifierrr",
            globals = c("img_fft")))

    if (verbose) message("Starting to apply the filters\n")

    time_taken <- Sys.time() - start_time

    if (verbose) message(
        "\nTook ", format(as.numeric(time_taken), digits = 2),
        " ", attr(time_taken, "units"),
        " to calculate the ", ncol(bound_flat),
        " features for ", nrow(bound_flat), " pixels\n")

    return(bound_flat)
}


#' @describeIn calc_features returns a named list of functions that can be applied to an image
#' @export
compile_calc_features <- function(filter_widths = c(3,5,11,23), img_dim, verbose = FALSE) {

    start_time <- Sys.time()
    if (verbose) message("Starting to compile features filters for image of dims {",
            paste(img_dim, collapse = ","),
            "}\n")

    if (verbose) message("Filters of size: {", paste0(filter_widths, collapse = ","), "}\n")

    filter_widths <- sort(filter_widths)

    g_funs <- purrr::map(
        filter_widths,
        ~ prep_filter.filter(
            EBImage::makeBrush(
                size = .x,
                shape = "Gaussian",
                sigma = ((.x/2) - 1)/3),
            img_dim))

    names(g_funs) <- paste0("gauss_filt_", filter_widths)

    dog_funs <- purrr::map(
        filter_widths,
        ~ compile_dog_filter(.x, img_dim))

    names(dog_funs) <- paste0("DoG_filt_", filter_widths)

    sobel_funs <- purrr::map(
        filter_widths,
        ~ compile_sobel_filter(width = .x, img_dim))

    names(sobel_funs) <- paste0("sobel_filt_", filter_widths)

    v_funs <- purrr::map(
        filter_widths,
        ~ compile_variance_filter(width = .x, img_dim))

    names(v_funs) <-  paste0("var_filt_", filter_widths)

    ret_funs <- c(g_funs, dog_funs, v_funs, sobel_funs)

    time_taken <- Sys.time() - start_time

    if (verbose) message("\nTook ", format(as.numeric(time_taken), digits = 2),
            " ", attr(time_taken, "units"),
            " to compile the ", length(ret_funs),
            " features\n")
    return(ret_funs)
}
