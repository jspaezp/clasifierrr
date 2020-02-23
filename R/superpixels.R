

#' Generate  superpixel mapping
#'
#' Performs a variation of kmeans to make virtual pixels.
#'
#' @param img input image file
#' @param num_superpixel number of superpixels
#' @param compactness parameter that denotes how similarly related the pixels must be
#'
#' @return list of 2 images, one with the averaged colors and one with the indexes
#' @export
#'
#' @examples
slic <- function(img, num_superpixel = 200,
                 compactness = 1,
                 init_centers = NULL, ...) {
    # Assuming a 2d image on grayscale
    scaling_spatial <- max(dim(img)*0.28)
    scaling_colour <- sd(as.matrix(img))

    ratio <- (scaling_spatial/scaling_colour)/(compactness*10)
    kmeans_mat <- reshape2::melt(as.matrix(img*ratio))

    if (is.null(init_centers)) {
        init_centers <- round(seq(1, length(img),length.out = num_superpixel))
        init_centers <- kmeans_mat[init_centers,]
    } else {
        center_pixels <- round(init_centers[, 1:2])
        center_pixels <- paste(kmeans_mat[,1], kmeans_mat[,2]) %in%
            paste(center_pixels[,1], center_pixels[,2])
        init_centers <- kmeans_mat[center_pixels, ]
    }

    start_time <- Sys.time()
    km <- kmeans(kmeans_mat, init_centers, ...)
    time_taken <- Sys.time() - start_time

    message(
        "Took ", format(as.numeric(time_taken), digits = 4),
        attr(time_taken, "units"), " to generate ", num_superpixel,
        " superpixels")

    segments_img <- EBImage::Image(km$cluster, dim = dim(img))
    superpixel_img <- EBImage::Image(km$centers[km$cluster, 3], dim = dim(img))
    superpixel_centers <- km$centers

    superpixel_img <- superpixel_img/ratio
    return(list(superpixel_img,
                superpixel_index = segments_img,
                superpixel_centers = superpixel_centers))
}



#' Classify images as superpixel slices
#'
#' @param img image to classify
#' @param features a features frame to be used for the classification
#' @param classifier a classifier
#' @param class_highlight a classiication to highlight if the prediction is categorical
#' @param ... additional arguments passed to kmeans
#'
#' @return
#' @export
#' @seealso slic
#'
#' @examples
#' # trainset <- build_train_multi(
#' # params_df,
#' # train_size_each = 5000,
#' # filter_widths = c(3,5))
#' #
#' # classifier <- ranger::ranger(pixel_class ~ ., trainset)
#' # features <- calc_features(base_img, c(3,5))
#' # sliced_img <- slic(sobel_filter(base_img, 5))
#' # classify_img_sliced(classifier, base_img, sliced_img[[2]],features, "sphere")
classify_img_sliced <- function(classifier, img, slices = slic(sobel_filter(img, 5))[[2]],
                                features, class_highlight, ...) {
    sliced_feats <- do.call(
        rbind, purrr::map(
            split(features, slices),
            ~ colMeans(.x)))

    pred <- predict_img(classifier, sliced_feats)
    pred <- highlight_category(
        class_mat = pred,
        class_highlight = class_highlight)
    pred_img <- EBImage::Image(
        pred[as.numeric(slices)],
        dim = dim(img))

    return(pred_img)
}



#' Assigns classifications to superpixels
#'
#' @param mask_list named list of masks
#' @param class_names optional if the list is not named
#' @param superpixel_index indexes for every pixel, as prvided by the second
#'                         element of`slic`
#' @param default_class
#'
#' @return a vector with same length as the number of superpixels and vales,
#'         the class of that superpixel
#' @export
#' @seealso slic
#'
#' @examples
#' spherid_mask <- readImageBw(system.file(
#'     "extdata", "4T1-shNT-1_layer2.png",
#'     package = "clasifierrr"))
#'
#' bg_mask <- readImageBw(system.file(
#'     "extdata", "4T1-shNT-1_layer1.png",
#'     package = "clasifierrr"))
#'
#'
#' base_image <- readImageBw(system.file(
#'     "extdata", "4T1-shNT-1.png",
#'     package = "clasifierrr"))
#'
#' segmented_img <- slic(
#'     sobel_filter(base_image, 5),
#'     compactness = 1,
#'     num_superpixel = 400)
#'
#' classifications <- get_superpixel_class(
#'     list("bg" = bg_mask, "spheroid" = spherid_mask),
#'     superpixel_index = segmented_img[[2]])
get_superpixel_class <- function(mask_list,
                                 class_names = names(mask_list),
                                 superpixel_index,
                                 default_class = "bg") {
    superpixel_tables <- purrr::map(
        mask_list,
        ~ table(
            as.factor(superpixel_index),
            as.logical(.x)))

    superpixel_pct <- purrr::map(
        superpixel_tables,
        ~  .x[,"TRUE"]/rowSums(.x))

    bound_tbl <- do.call(cbind, superpixel_pct)

    which_most <- apply(bound_tbl, 1, which.max)
    ambiguous <- apply(bound_tbl, 1, function(...) length(unique(...)) == 1 )

    classes <- character(length = nrow(bound_tbl))
    classes <- class_names[which_most]
    classes[ambiguous] <- default_class
    names(classes) <- rownames(bound_tbl)
    return(classes)
}



#' Calculates attributes on the pixels contained within each superpixel
#'
#' Returs a table of "number of superpixel" rows and a column for each
#' calculated feature, plus an additional column with the classification of
#' such pixel.
#'
#' @inheritParams get_superpixel_class
#' @param base_image img
#'
#' @return data.frame of features and pixel classes
#' @export
#'
#' @examples
#' @importFrom EBImage computeFeatures
calc_superpixel_feats <- function(mask_list,
                                  superpixel_index,
                                  base_image,
                                  class_names = names(mask_list),
                                  default_class = "bg") {
    classifications <- get_superpixel_class(
        mask_list = mask_list,
        superpixel_index = superpixel_index,
        default_class = "bg")

    sliced_features <- EBImage::computeFeatures(
        superpixel_index,
        base_image,
        methods.noref = NULL,
        methods.ref = c("computeFeatures.basic", "computeFeatures.haralick"))

    trainset <- as.data.frame(sliced_features)
    trainset$pixel_class <- classifications
    return(trainset)
}


