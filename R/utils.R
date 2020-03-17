

#' Makes sure a number is odd
#'
#' Since many of the 2d filtering functions rely on having odd numbers for the
#' kernell sizes, it makes sense to have a function that assures numbers being
#' odd, this functions gets a number, makes it a integer and adds 1 if the
#' number is even, then removes duplicates.
#'
#' @param num numeric vector
#'
#' @return
#' @export
#'
#' @examples
make_odd <- function(num) {
    num <- as.integer(num)
    num <- num + ((num + 1) %% 2)
    num <- as.integer(unique(num))
    num <- num[num > 0]
    return(num)
}


#' Generate an overlaid image of a mask and a base image
#'
#' @param img image
#' @param mask mask
#' @param opac numeric vector o length 2 with the opacity of the overlaid colours
#' @param cols color of the outline and fill that will be used
#'
#' @return
#' @export
#'
#' @examples
#' x = readImage(system.file('images', 'shapes.png', package='EBImage'))
#' sobel <- sobel_filter(x)
#' overlay <- generate_overlay(x, sobel < 0.5)
#' display(overlay, "raster")
#' display(generate_overlay(x, EBImage::fillHull(sobel > 0.5)), "raster")
#' @importFrom viridisLite viridis
#' @importFrom EBImage Image Color paintObjects
generate_overlay <- function(img, mask,
                             opac = c(1, 0.2),
                             cols =  c("#FDE725FF", "#440154FF")) {
    disp_img <- EBImage::Image(
        c(img, img, img),
        dim = c(dim(img), 3),
        colormode = EBImage::Color)

    overlaid <- EBImage::paintObjects(
        mask, disp_img,
        col = cols,
        opac = opac,
        thick = TRUE)
    return(overlaid)
}

#' Generate an edge image of a mask, based on an image
#'
#' @inheritParams generate_overlay
#'
#' @return
#' @export
#'
#' @examples
#' x = readImage(system.file('images', 'shapes.png', package='EBImage'))
#' sobel <- sobel_filter(x)
#' edges <- generate_edges(x, sobel < 0.5)
#' display(edges, "raster")
#' display(generate_edges(x, EBImage::fillHull(sobel > 0.5)), "raster")
#' @importFrom viridisLite viridis
#' @importFrom EBImage Image Color paintObjects
generate_edges <- function(img, mask,
                           opac = c(1, 0.2),
                           cols = c("#440154FF", "#FDE725FF")) {
    disp_img <- EBImage::Image(
        c(img, img, img),
        dim = c(dim(img), 3),
        colormode = EBImage::Color)

    edges <- EBImage::paintObjects(
        mask,
        EBImage::Image(
            rep(1, length(disp_img)),
            dim = c(dim(disp_img)),
            colormode = Color),
        col = cols[[1]],
        thick = TRUE
    )

    return(edges)
}


#' Makes all the values in the edges of a matrix or image a value
#'
#' @param img image to use
#' @param clean_size number of rows or cols to remove on each edge
#' @param val value to be used to replace 0
#'
#' @return
#' @export
#'
#' @examples
#' x = readImage(system.file('images', 'shapes.png', package='EBImage'))
#' sobel <- sobel_filter(x, 15)
#' display(sobel, "raster")
#' display(clean_image_border(sobel, 50, 0))
#' @importFrom viridisLite viridis
#' @importFrom EBImage Image Color paintObjects
clean_image_border <- function(img, clean_size, val = 0) {
    rows <- nrow(img)
    cols <- ncol(img)

    rows_clean <- c(1:clean_size, (rows - clean_size):rows)
    cols_clean <- c(1:clean_size, (cols - clean_size):cols)
    img[rows_clean, ] <- val
    img[, cols_clean] <- val
    return(img)
}

