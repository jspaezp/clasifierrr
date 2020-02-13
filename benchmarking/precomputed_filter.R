
devtools::load_all("~/git/clasifierrr/")

f = system.file("images", "sample.png", package = "EBImage")
f = system.file(
  "extdata", "4T1-shNT-1.png",
  package = "clasifierrr")
img = EBImage::readImage(f)
my_filter <- EBImage::makeBrush(31, "gaussian", sigma = 4)


full_calc_precomp_circular <- function() {
    prepd_filt <- prep_filter.filter(filter = my_filter, dim_x = dim(img))
    fft_img <- fftwtools::fftw2d(img)

    # Image preprocessing can be skiped if using circular to handle boundries...
    filtered_img <- filter2.prepared(
        prep_x = img,
        fft_x = fft_img,
        dims_x = dim(img),
        dims_x_orig = dim(img),
        x_dimnames = dimnames(img),
        prep_filter = prepd_filt)
    return(prepd_filt)
}

setup_precomp_cirtcular <- function(){
    prepd_filt <- prep_filter.filter(filter = my_filter, dim_x = dim(img))
    fft_img <- fftwtools::fftw2d(img)

}

prepd_filt <- prep_filter.filter(filter = my_filter, dim_x = dim(img))
fft_img <- fftwtools::fftw2d(img)

partial_calc_precomp_circular <- function() {
    filtered_img <- filter2.prepared(
        prep_x = img,
        fft_x = fft_img,
        dims_x = dim(img),
        dims_x_orig = dim(img),
        x_dimnames = dimnames(img),
        prep_filter = prepd_filt)
    return(prepd_filt)

}

full_calc_std_circular <- function() {
    oldschool_filtered <- EBImage::filter2(img, my_filter)
}

full_calc_precomp_replicate <- function() {

    prepd_img <- prep_filter.img(
        img, filter_dims = dim(my_filter),
        boundary = "replicate")

    prepd_filt <- prep_filter.filter(filter = my_filter, dim_x_proc = attr(prepd_img, "dx"))

    fft_img <- fftwtools::fftw2d(prepd_img)


    filtered_img <- filter2.prepared(
        prep_x = prepd_img,
        raw_x = img,
        fft_x = fft_img,
        dims_x = attr(prepd_img, "dx"),
        dims_x_orig = attr(prepd_img, "d"),
        prep_filter = prepd_filt,
        boundary = "replicate")

   return(filtered_img)
}


setup_replicate_precalc <- function() {
    prepd_img_rep <- prep_filter.img(
        img, filter_dims = dim(my_filter),
        boundary = "replicate")
    prepd_filt_rep <- prep_filter.filter(
        filter = my_filter, dim_x_proc = attr(prepd_img_rep, "dx"))
    fft_img_rep <- fftwtools::fftw2d(prepd_img_rep)

}


prepd_img_rep <- prep_filter.img(
    img, filter_dims = dim(my_filter),
    boundary = "replicate")
prepd_filt_rep <- prep_filter.filter(filter = my_filter, dim_x_proc = attr(prepd_img_rep, "dx"))
fft_img_rep <- fftwtools::fftw2d(prepd_img_rep)


partial_calc_precomp_replicate <- function() {
    filtered_img <- filter2.prepared(
        prep_x = prepd_img_rep,
        raw_x = img,
        fft_x = fft_img_rep,
        dims_x = attr(prepd_img_rep, "dx"),
        dims_x_orig = attr(prepd_img_rep, "d"),
        prep_filter = prepd_filt_rep,
        boundary = "replicate")

   return(filtered_img)
}


full_calc_std_replicate <- function() {
  oldschool_filtered <- EBImage::filter2(img, my_filter, boundary = "replicate")
  return(oldschool_filtered)
}


require(microbenchmark)

mb1 <- microbenchmark::microbenchmark(
    full_calc_precomp_circular(),
    partial_calc_precomp_circular(),
    full_calc_std_circular(),
    setup_precomp_cirtcular(),
    times = 20
)


mb2 <- microbenchmark::microbenchmark(
    full_calc_std_replicate(),
    partial_calc_precomp_replicate(),
    full_calc_precomp_replicate(),
    setup_replicate_precalc(),
    times = 20
)

require(ggplot2)

mb1
mb2
autoplot(mb1, log = FALSE)
autoplot(mb2, log = FALSE)

prepd_filt_mb = function() { prep_filter.filter(filter = my_filter, dim_x = dim(img)) }
fft_img_mb = function() { fftwtools::fftw2d(img) }
prepd_img_rep_mb = function() { prep_filter.img( img, filter_dims = dim(my_filter), boundary = "replicate") }
prepd_img_circ_mb = function() { prep_filter.img( img, filter_dims = dim(my_filter), boundary = "circular") }

mb3 <- microbenchmark(
    prepd_filt_mb(),
    fft_img_mb(),
    prepd_img_rep_mb(),
    prepd_img_circ_mb(),
    times = 100
)

autoplot(mb3, log = TRUE)
mb3

large_img <- EBImage::abind(purrr::map(1:16, ~ img), along = 1)

lazy_filtered <- function() { lazy_filter2(large_img, my_filter, boundary = "replicate") }
oldschool_filtered <- function() { EBImage::filter2(large_img, my_filter, boundary = "replicate") }

system.time({
  lazy_filtered()
})

system.time({
  lazy_filtered()
})

oldschool_filtered()

memoise::forget(clasifierrr::mem_fft)
memoise::forget(clasifierrr::mem_prep_filter.img)



mb4 <- microbenchmark(
  lazy_filtered(),
  oldschool_filtered(),
  times = 10
)

mb5 <- microbenchmark(
  lazy_filtered(),
  oldschool_filtered(),
  times = 10
)


autoplot(mb4, log = TRUE)
mb4


autoplot(mb5, log = TRUE)
mb5


xx <- filter2_circular(img = img, filter = my_filter, fft_img = fft_img)
yy <- EBImage::filter2(x = img, filter = my_filter)
testthat::expect_equal(xx, yy)
