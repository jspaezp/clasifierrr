test_that("filter calculation with template works", {
  base_image <- readImageBw(system.file(
              "extdata", "tiny_4T1-shNT-1.png",
              package = "clasifierrr"))
  features <- calc_features(
      base_image, filter_widths = c(3,5),
      shape_sizes = c(15, 31, 51))
  expect_gt(length(dim(features)), 1)
})

test_that("muti image feature calculation works", {
  params_df <- tibble::tibble(
      file = c(
          system.file(
              "extdata", "tiny_4T1-shNT-1_layer1.png",
              package = "clasifierrr"),
          system.file(
              "extdata", "tiny_4T1-shNT-1_layer2.png",
              package = "clasifierrr")),
      classif = c("spheroid", "bg"),
      related_file = system.file(
          "extdata", "tiny_4T1-shNT-1.png",
          package = "clasifierrr")
  )
  trainset <- build_train_multi(
      params_df, filter_widths = c(3,5),
      shape_sizes = c(15, 31, 51))
  expect_gt(length(dim(trainset)), 1)
})