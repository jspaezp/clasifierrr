# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


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

    magx <- EBImage::filter2(img, sobel_kern$x)
    magy <- EBImage::filter2(img, sobel_kern$y)

    ret <- sqrt( magx^2 + magy^2 )
    return(ret)
}


#' Title
#'
#' @param img
#' @param chunk_width
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom EBImage makeBrush filter2 Grayscale
#' @importFrom reshape2 melt
correct_light <- function(img, chunk_width) {
    tmp_brush <- EBImage::makeBrush(151, "disc")
    light_approx <- EBImage::filter2(img, tmp_brush/sum(tmp_brush))

    lm_data <- reshape2::melt(as.matrix(light_approx))

    my_lm <- lm(data = lm_data, value ~ Var1 * Var2)
    pred_lm <- matrix(ncol = ncol(light_approx), nrow = nrow(light_approx))
    pred_lm <- reshape2::melt(pred_lm)
    pred_lm$value <- predict(my_lm, newdata = pred_lm)

    light <- EBImage::Image(
        reshape2::acast(pred_lm, Var1 ~ Var2),
        colormode = EBImage::Grayscale)
    img <- img - light
    img[img < 0] <- 0
    return(img)
}


#' Filters small or large objects
#'
#' Filters out in a mask objects that are too little or too large
#'
#' @param mask an image object of type mask
#' @param min_radius minimum radius of objects to keep
#' @param max_radius maximum radius of objects to keep
#'
#' @return
#' @export img
#'
#' @examples
#' @importFrom EBImage bwlabel computeFeatures.shape
filter_masks <- function(mask, min_radius, max_radius) {
    mask <- EBImage::bwlabel(mask)
    shape_feats <- EBImage::computeFeatures.shape(mask)

    removable_objs <- rownames(shape_feats)[
        shape_feats[,"s.radius.min"] < min_radius |
            shape_feats[,"s.radius.max"] > max_radius]

    mask <- EBImage::rmObjects(mask, index = as.character(removable_objs))
    return(mask)
}


#generate_filters <- function(img, filter_list) {
#
#}

#' Read 2d images
#'
#' Similar to EBImage::readImage but forces it to be black and white by adding
#' all the channels and dividing them by the number of channels
#'
#' @param path file path to read
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom EBImage Image Grayscale readImage
readImageBw <- function(path){
    img <- EBImage::readImage(
        path)

    if (length(dim(img)) == 3){
        img <- EBImage::Image(
            apply(img, c(1,2), sum)/(dim(img)[3]),
            colormode = EBImage::Grayscale)
    }

    return(img)
}



#' Calculate filter features
#'
#' Calculates features for each pixel based on sobel filters, gaussian and
#' difference of gaussians, adds as well the position in the image
#'
#' @param img
#' @param filter_widths
#'
#' @return data.frame
#' @export
#'
#' @examples
#' @importFrom purrr map map_dfc
#' @importFrom EBImage Image makeBrush filter2
calc_features <- function(img, filter_widths = c(3,5,11,23)){
    s_filtered <- purrr::map(
        filter_widths,
        ~ sobel_filter(img, width = .x))

    names_s_filtered <- paste0("sobel_filt_", filter_widths)

    g_filtered <- purrr::map(
        filter_widths,
        ~ EBImage::filter2(img, EBImage::makeBrush(size = .x, "gaussian")))

    names_g_filtered <- paste0("gauss_filt_", filter_widths)

    g_diff <- purrr::map2(
        seq_along(g_filtered)[-1], seq_along(g_filtered)[-length(g_filtered)],
        ~ g_filtered[[.x]] - g_filtered[[.y]])

    names_g_diff <- paste0("gauss_diff_", filter_widths[-length(g_filtered)])

    position <- list(
        y_position = EBImage::Image(
            (seq_along(img) %/% ncol(img))/nrow(img),
            dim = dim(img)),
        x_position = EBImage::Image(
            (seq_along(img) %% nrow(img))/ncol(img),
            dim = dim(img)))

    bound_flat <- purrr::map_dfc(
        c(s_filtered, g_filtered, g_diff, position),
        as.numeric)
    names(bound_flat) <- c(names_s_filtered, names_g_filtered,
                           names_g_diff, names(position))
    return(bound_flat)
}


get_class <- function(file, classif, ...) {
    # > foo <- get_class("raw_data/4T1-shNT-1_layer1.png", "class1")
    # > str(foo)
    # List of 4
    # $ class : chr "class1"
    # $ file  : chr "raw_data/4T1-shNT-1_layer1.png"
    # $ pixels: int [1:108470] 83463 83464 83465 83466 83467 84485 84486 84487 84488 84489 ...
    # $ dims  : int [1:2] 1024 768
    img <- readImageBw(file)
    pix_class <- which(img != 0)

    print(paste(
        "Returning for file: ", file,
        "and classification", classif,
        "a total of", length(pix_class),
        "positive pixels"))
    return(list(
        class = classif,
        file = file,
        pixels = pix_class,
        dims = dim(img)))
}



#' Title
#'
#' imgs shouls be a data frame of files and classifications and related images
#'
#' @param imgs_df data frame with 3 columns, file, classif and related_file
#'
#' @return
#' @export
#'
#' @examples
#' in_df <- data.frame(
#'   file = c("image1_class1.png", "image1_class2.png"),
#'   classif = c("class1", "class2"),
#'   related_file = "image1.png"
#' )
get_classifications <- function(imgs_df) {
    #
    # data.frame(
    #   file = c("image1_class1.png", "image1_class2.png"),
    #   classif = c("class1", "class2"),
    #   related_file = "image1.png"
    # )

    # Dims for matrices are c(nrow(img), ncol(img))
    classes <- purrr::pmap(imgs_df, get_class)

    # This generates a lisf of n(number of files)
    split_classes <- split(classes, imgs_df$related_file)

    # Now we generate a character vector for each of the files
    consolidate_classes <- function(class_lists){
        pixel_class <- character(length = class_lists[[1]]$dims[1]*class_lists[[1]]$dims[2])
        for (pix_class in class_lists) {
            pixel_class[pix_class$pixels] <- pix_class$class
        }
        return(pixel_class)
    }

    consolidated_classes <- purrr::map(split_classes, consolidate_classes)
    names(consolidated_classes) <- names(split_classes)
    return(consolidated_classes)
}


build_train <- function(feat_img, pixel_classes, train_size = 50000) {
    bound_flat <- as.data.frame(feat_img)
    bound_flat$pixel_class <- pixel_classes

    train_pixels <- sample(which(bound_flat$pixel_class != ""), size = train_size)
    train_data <- bound_flat[train_pixels,]
    return(train_data)
}

build_train_multi <- function(imgs_df, train_size_each = 50000) {
    classes_list <- get_classifications(imgs_df)

    trainset <- purrr::map2_df(
        names(classes_list), classes_list,
        ~ build_train(calc_features(readImageBw(.x)), .y, train_size_each)
    )

    print(table(trainset$pixel_class))
    print(dim(trainset))
    return(trainset)

}

test_consolidate_classes <- function(){
    params_df <- tibble::tibble(
        file = c("raw_data/4T1-shNT-1_layer1.png", "raw_data/4T1-shNT-1_layer2.png"),
        classif = c("spheroid", "bg"),
        related_file = "raw_data/4T1-shNT-1.png"
    )
    classes <- get_classifications(params_df)
}

test_build_train_multi <- function() {
    params_df <- tibble::tibble(
        file = c("raw_data/4T1-shNT-1_layer1.png", "raw_data/4T1-shNT-1_layer2.png"),
        classif = c("spheroid", "bg"),
        related_file = "raw_data/4T1-shNT-1.png"
    )
    trainset <- build_train_multi(params_df)
}



#' Title
#'
#' @param classifier
#' @param path
#' @param img
#' @param feature_frame
#' @param class_highlight classifier to show as white
#' @param dims
#'
#' @return
#' @export
#'
#' @examples
classify_img <- function(classifier, path = NULL, img = NULL, feature_frame = NULL,
                         class_highlight = unique(classifier$predictions)[[1]],
                         dims = NULL){

    if (is.null(img) & is.null(feature_frame)) {
        message("Attempting to read image from file")
        img <- readImageBw(path)
        dims <- dim(img)
    }

    if (is.null(feature_frame)) {
        message("Attempting to calculate features")
        feature_frame <- calc_features(img)
        dims <- dim(img)
    }

    stopifnot(!is.null(dims))

    message("Starting classification")
    start_time <- Sys.time()
    prediction <- predict(classifier, feature_frame)
    time_taken <- Sys.time() - start_time

    message(
        paste("Took",
        format(as.numeric(time_taken), digits = 4),
        attr(time_taken, "units"),
        "to predict the image"))
    pred_mat <- prediction$predictions

    out_img <- EBImage::Image(as.numeric(pred_mat == class_highlight), dim = dims)
    return(out_img)
}
