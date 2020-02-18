


#' Generate an overlaid image of a mask and a base image
#'
#' @param img image
#' @param mask mask
#' @param opac
#'
#' @return
#' @export
#'
#' @examples
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
#' @param img image
#' @param mask mask
#' @param opac
#'
#' @return
#' @export
#'
#' @examples
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



