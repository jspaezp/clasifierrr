
require(furrr)
require(future)

devtools::load_all()

filter_widths <- c(3,5,7,11)
base_image <- readImageBw(system.file(
    "extdata", "tiny_4T1-shNT-1.png",
    package = "clasifierrr"))

message("\n\n\n\nRunning for tiny image\n\n\n\n")
plan(sequential)
features <- calc_features(base_image, filter_widths)
print(head(features))

plan(multiprocess)
features <- calc_features(base_image, filter_widths = filter_widths)
print(head(features))

display_filters(features, dim(base_image))

summary(features$gauss_diff_3 - features$gauss_filt_5)

base_image <- readImageBw(system.file(
    "extdata", "4T1-shNT-1.png",
    package = "clasifierrr"))

message("\n\n\n\nRunning for normal sized image\n\n\n\n")
plan(sequential)
features <- calc_features(base_image, filter_widths = filter_widths)
print(head(features))

plan(multiprocess)
features <- calc_features(base_image, filter_widths = filter_widths)
print(head(features))