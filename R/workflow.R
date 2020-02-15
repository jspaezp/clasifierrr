

#' Corrects uniform changes in light
#'
#' @param img an image
#' @param chunk_width the chunk sized used to calculate background light
#'
#' The size of the chunk should be large enough that each chunk contains at
#' least a part of background.
#'
#' Internally estimates the light in 2d and aproximates a linear model to it,
#' then subtracts the estimated light.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom EBImage makeBrush filter2 Grayscale
#' @importFrom reshape2 melt
correct_light <- function(img, chunk_width = 200) {
    tmp_brush <- EBImage::makeBrush(chunk_width, "disc")
    light_approx <- EBImage::filter2(
        img, tmp_brush/sum(tmp_brush),
        boundary = "replicate")

    lm_data <- reshape2::melt(as.matrix(light_approx))

    my_lm <- lm(data = lm_data, value ~ Var1 * Var2)
    pred_lm <- matrix(ncol = ncol(light_approx), nrow = nrow(light_approx))
    pred_lm <- reshape2::melt(pred_lm)
    pred_lm$value <- predict(my_lm, newdata = pred_lm)

    light <- EBImage::Image(
        reshape2::acast(pred_lm, Var1 ~ Var2),
        colormode = EBImage::Grayscale)
    img <- img - light
    # img[img < 0] <- 0
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
#' @export
#'
#' @examples
#' img <- readImageBw(system.file("extdata", "tiny_4T1-shNT-1.png", package = "clasifierrr"))
#' mask <- EBImage::thresh(img)
#'
#' # EBImage::display(mask)
#'
#' # EBImage::display(filter_masks(mask, min_radius = 2, max_radius = Inf))
#' # EBImage::display(filter_masks(mask, min_radius = 0, max_radius = 20))
#'
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

    if (length(dim(img)) == 3) {
        img <- EBImage::Image(
            apply(img, c(1,2), sum)/(dim(img)[3]),
            colormode = EBImage::Grayscale)
    }

    return(img)
}



get_class <- function(file, classif,
                      preprocess_fun_mask = NULL,
                      ...) {
    # > foo <- get_class("raw_data/4T1-shNT-1_layer1.png", "class1")
    # > str(foo)
    # List of 4
    # $ class : chr "class1"
    # $ file  : chr "raw_data/4T1-shNT-1_layer1.png"
    # $ pixels: int [1:108470] 83463 83464 83465 83466 83467 84485 84486 84487 84488 84489 ...
    # $ dims  : int [1:2] 1024 768
    img <- readImageBw(file)

    if (!is.null(preprocess_fun_mask)) {
        img <- preprocess_fun_mask(img)
    }
    pix_class <- which(img != 0)

    message(paste(
        "Returning for file: ", file,
        "and classification\"", classif,
        "\" a total of {", length(pix_class),
        "} positive pixels"))
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
#' @param preprocess_fun_mask optional preprocessing function to be used on the images
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
get_classifications <- function(imgs_df,
                                preprocess_fun_mask = NULL) {
    # Dims for matrices are c(nrow(img), ncol(img))
    classes <- purrr::pmap(
        imgs_df,
        get_class,
        preprocess_fun_mask = preprocess_fun_mask)

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

    valid_pixels <- which(bound_flat$pixel_class != "")

    if (train_size > length(valid_pixels)) {
        warning(
            "The selected train size(",
            train_size, ") is larger than the number of classified ",
            "pixels (", length(valid_pixels),
            ") ", " so the number is getting updated to the total ",
            "number of available pixels\n")
        train_size <- length(valid_pixels)
    }

    train_pixels <- sample(valid_pixels, size = train_size)
    train_data <- bound_flat[train_pixels,]
    return(train_data)
}


#' Calculates a data frame to train classifiers
#'
#' @param imgs_df a data frame specifying the masks and images to use, see details
#' @param train_size_each number of elements to keep for each mask/image combination
#' @param preprocess_fun_img optional, processing function to be applied to each image
#' @param preprocess_fun_mask optional, processing function to be applied to each mask
#' @param filter_widths numeric vector with the filter widths to be used
#'
#' @return data.frame
#' @export
#'
#' @examples
#' params_df <- tibble::tibble(
#'     file = c(
#'         system.file(
#'             "extdata", "tiny_4T1-shNT-1_layer1.png",
#'             package = "clasifierrr"),
#'         system.file(
#'             "extdata", "tiny_4T1-shNT-1_layer2.png",
#'             package = "clasifierrr")),
#'     classif = c("spheroid", "bg"),
#'     related_file = system.file(
#'         "extdata", "tiny_4T1-shNT-1.png",
#'         package = "clasifierrr")
#'     )
#' trainset <- build_train_multi(
#'     params_df,
#'     train_size_each = 5000,
#'     filter_widths = c(3,5))
build_train_multi <- function(imgs_df,
                              train_size_each = 50000,
                              preprocess_fun_img = NULL,
                              preprocess_fun_mask = NULL,
                              filter_widths = c(3, 5, 15, 31)) {

    if (is.null(preprocess_fun_mask)) {
        preprocess_fun_mask = (function(x){x})
    }

    if (is.null(preprocess_fun_img)) {
        preprocess_fun_img = (function(x){x})
    }

    classes_list <- get_classifications(
        imgs_df,
        preprocess_fun_mask = preprocess_fun_mask)

    trainset <- purrr::map2_df(
        names(classes_list), classes_list,
        ~ build_train(
          feat_img = calc_features(
              preprocess_fun_img(readImageBw(.x)),
              filter_widths = filter_widths),
          pixel_classes = .y,
          train_size = train_size_each)
    )

    tbl <- table(trainset$pixel_class)
    message(
        "Classified objects are of classes {",
        paste(
            names(tbl),
            as.numeric(tbl),
            sep = ": ",
            collapse = "} and {"),
        "}\n\n")

    message(
        "Returning a data frame of ",
        nrow(trainset),
        " rows and ", ncol(trainset),
        " columns")
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



#' Classify Images
#'
#' @param classifier an object returned from ranger to use as a classifier
#' @param path the path of the image to  classify
#' @param img an EBImage image
#' @param feature_frame a data frame with the calculated features
#' @param filter_widths filters widths to use when calculating features
#' @param class_highlight classifier to show as white in the final mask
#' @param dims dimensions of the images to classify, only required if using the feature_frmae interface
#'
#' This classifier by default will fall back to file, trying in order of
#' priority a `feature_frame` > `img` > `path`, please note that since the
#' `feature_frame` does not contain the dimensions of the image, it has to be
#' provided in the `dims` argument in addition.
#'
#' @return
#' @export
#'
#' @examples
#' params_df <- tibble::tibble(
#'     file = c(
#'         system.file(
#'             "extdata", "tiny_4T1-shNT-1_layer1.png",
#'             package = "clasifierrr"),
#'         system.file(
#'             "extdata", "tiny_4T1-shNT-1_layer2.png",
#'             package = "clasifierrr")),
#'     classif = c("spheroid", "bg"),
#'     related_file = system.file(
#'         "extdata", "tiny_4T1-shNT-1.png",
#'         package = "clasifierrr")
#'     )
#' trainset <- build_train_multi(params_df, filter_widths = c(3, 5, 15))
#' trainset$pixel_class <- trainset$pixel_class == "spheroid"
#' model_simple_glm <- glm(pixel_class~.,data = trainset, family = binomial(link = "logit"))
#' class_img <- classify_img(
#'     model_simple_glm, path = params_df[[3]][[1]],
#'     filter_widths = c(3, 5, 15))
#' # plot(as.raster(class_img))
classify_img <- function(classifier, path = NULL, img = NULL,
                         feature_frame = NULL, filter_widths = NULL,
                         class_highlight = NULL,
                         dims = NULL, preprocess_fun_img = NULL){

    if (is.null(img) & is.null(feature_frame)) {
        message("Attempting to read image from file", path)
        img <- readImageBw(path)
        if (!is.null(preprocess_fun_img)) {
            message("Applying image preprocessing")
            img <- preprocess_fun_img(img)
        }
        dims <- dim(img)
    }

    if (is.null(feature_frame)) {
        message("Attempting to calculate features")
        stopifnot(!is.null(filter_widths))
        feature_frame <- calc_features(img, filter_widths = filter_widths)
        dims <- dim(img)
    }

    stopifnot(!is.null(dims))

    message("Starting classification")
    start_time <- Sys.time()
    pred_mat <- predict_img(classifier, feature_frame)
    time_taken <- Sys.time() - start_time

    message(
        paste("Took",
        format(as.numeric(time_taken), digits = 4),
        attr(time_taken, "units"),
        "to predict the image"))

    classifier_discrete <- inherits(
        pred_mat,
        c("factor", "character"))


    if (classifier_discrete) {
        unique_vals <- unique(pred_mat)
        if (length(unique_vals) != 2) {
            warning("The classification had {", length(unique_vals),
                    "} distinct values as an output, ",
                    "make sure that is what you want")
        }

        if (!is.null(class_highlight)) {
            if (!class_highlight %in% unique_vals) {
                warning(
                    "{", class_highlight, "}",
                    " was not found in the predicted matrix",
                    " will just use the first one found {",
                    unique(pred_mat)[[1]],
                    "}\n")
                class_highlight <- unique(pred_mat)[[1]]
            }
            pred_mat <- as.numeric(pred_mat == class_highlight)
        }
    } else {
        if (!is.null(class_highlight)) {
            warning("The output classifier is not discrete so",
                    " the provided 'class_highlight' variable {",
                    class_highlight, "} will be dissregarded\n")
        }
    }

    pred_mat <- as.numeric(pred_mat)
    if (max(pred_mat) > 1 | min(pred_mat) < 0) {
        warning("Found in the final classification {",
                sum(pred_mat > 1), "} values more than 1 and {",
                sum(pred_mat < 0), "} values less than 0, ",
                "This might be undesired in the final image and lead",
                " to inconsistencies\n")
    }
    out_img <- EBImage::Image(pred_mat, dim = dims)
    return(out_img)
}

predict_img <- function(x, ...) {
    UseMethod("predict_img")
}

predict_img.glm <- function(x, feature_frame) {
    prediction <- predict(x, feature_frame, type = "response")
    return(prediction)
}

predict_img.ranger <- function(x, feature_frame) {
    prediction <- ranger:::predict.ranger(x, data = feature_frame)
    pred_mat <- prediction$predictions
    return(pred_mat)
}

predict_img.ksvm <- function(x, feature_frame) {
    prediction <- kernlab::predict(x, feature_frame)
    return(prediction)
}


predict_img.default <- function(x, feature_frame) {
    prediction <- predict(x, feature_frame)
    return(prediction)
}

predict_img.glmnet <- function(x, feature_frame) {
    predict_img.default(x, as.matrix(feature_frame))
}


#' Displays the data from a feature frame
#'
#' @param feature_df a data frame with the calculated features, usually from `calc_features`
#' @param dims the original dimensions of the image
#' @param scale defaults to `FALSE`
#'
#' @return
#' @export
#' @seealso calc_features
#'
#' @examples
#' myfile <- system.file("extdata", "4T1-shNT-1.png", package = "clasifierrr")
#' myimg <- readImageBw(myfile)
#' myfeat <- calc_features(myimg, c(3,5))
#' display_filters(myfeat, dim(myimg))
display_filters <- function(feature_df, dims, scale = FALSE) {

    if (scale) {
        feature_df <- lapply(feature_df, function(...) { scale(...) + 0.5 } )
    }

    image_full_list <- purrr::map(
        names(feature_df),
        ~ EBImage::Image(feature_df[[.x]], dims))
    comb_img <- EBImage::combine(image_full_list)
    EBImage::display(
        comb_img,
        method = "raster",
        all = TRUE)
    return(invisible(comb_img))
}
