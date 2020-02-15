test_that("Prepared filter outputs the same as EBImage implementation", {
  f = system.file("images", "sample.png", package = "EBImage")
  img = EBImage::readImage(f)
  my_filter <- EBImage::makeBrush(31, "gaussian", sigma = 4)

  prepd_filt <- prep_filter.filter(filter = my_filter, dim_x_proc = dim(img))
  fft_img <- fftwtools::fftw2d(img)

  # Image preprocessing can be skiped if using circular to handle boundries...
  filtered_img <- filter2.prepared(
    prep_x = img,
    fft_x = fft_img,
    dims_x = dim(img),
    dims_x_orig = dim(img),
    x_dimnames = dimnames(img),
    prep_filter = prepd_filt)

  oldschool_filtered <- EBImage::filter2(img, my_filter)

  expect_equal(filtered_img, oldschool_filtered)

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

  oldschool_filtered <- EBImage::filter2(img, my_filter, boundary = "replicate")

  expect_equal(filtered_img, oldschool_filtered)

})
