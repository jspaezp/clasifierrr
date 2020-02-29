

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
#' x = readImage(system.file('images', 'shapes.png', package='EBImage'))
#' superpixelated <- slic(x)
#'
#' # # To display the average color of each superpixel
#' display(superpixelated[[1]], "raster")
#' # # To display a colored version that makes evident the superpixel separations
#' display(EBImage::colorLabels(superpixelated[[2]]), "raster")
slic <- function(img, num_superpixel = 200, compactness = 1) {
    # Assuming a 2d image on grayscale
    scaling_spatial <- max(dim(img)*0.28)
    scaling_colour <- sd(as.matrix(img))

    ratio <- (scaling_spatial/scaling_colour)/(compactness*10)
    kmeans_mat <- reshape2::melt(as.matrix(img*ratio))

    init_centers <- round(seq(1, length(img),length.out = num_superpixel))

    start_time <- Sys.time()
    km <- kmeans(kmeans_mat, kmeans_mat[init_centers,], iter.max = 10)
    time_taken <- Sys.time() - start_time

    message(
        "Took ", format(as.numeric(time_taken), digits = 4),
        attr(time_taken, "units"), " to generate ", num_superpixel,
        " superpixels")

    segments_img <- EBImage::Image(km$cluster, dim = dim(img))
    superpixel_img <- EBImage::Image(km$centers[km$cluster, 3], dim = dim(img))

    superpixel_img <- superpixel_img/ratio
    return(list(superpixel_img, superpixel_index = segments_img))
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
