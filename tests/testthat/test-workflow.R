test_that("multiplication works", {
  expect_success({

      img_file <- system.file(
          "extdata", "raw_data/4T1-shNT-1",
          package = "classifierrr")
      img <- EBImage::readImage(
          "./raw_data/4T1-shNT-1.png")
      img <- Image(apply(img, c(1,2), sum)/3, colormode = Grayscale)
      display(img)
      c_img <- correct_light(img)

      otsu_img <- c_img > otsu(c_img) + 0.01
      otsu_img <- bwlabel(otsu_img)
      dilate_otsu <- dilate(otsu_img, makeBrush(51, "disc"))
      fill_otsu <- fillHull(dilate_otsu)
      display((fill_otsu))
      filtered_dilat_otsu <- filter_masks(fill_otsu, 50, 2000)
      display(filtered_dilat_otsu)
      c_img2 <- c_img
      c_img2[filtered_dilat_otsu == 0] <- 0
      display(c_img)
      display(c_img2 > otsu(c_img2))

  })
})
