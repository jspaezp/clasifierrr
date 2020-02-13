

f <- system.file(
    "extdata", "4T1-shNT-1.png",
    package = "clasifierrr")
img <- EBImage::readImage(f)
large_img <- EBImage::abind(purrr::map(1:2, ~ img), along = 1)


devtools::load_all("~/git/clasifierrr/.")

prof1_res <- profvis::profvis({ calc_features(large_img, filter_widths = c(3,5,7,11)) })

devtools::load_all("~/git/clasifierrr_master/.")

prof2_res <- profvis::profvis({  calc_features(large_img, filter_widths = c(3,5,7,11)) })


prof1_res
prof2_res

