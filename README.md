
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clasifierrr

<!-- badges: start -->

<!-- badges: end -->

The goal of clasifierrr is to …

## Installation

``` r
remotes::install_github("jspaezp/clasifierrr")
```

## Workflow

1.  Externally create the masking files for the classification
2.  Internally create a parameters data frame
3.  use `build_train_multi` to create the training dataset
4.  use `ranger` to train the classifier
5.  use `classify_img` to classify as many images as you want
6.  (optional) use `filter_masks` to remove objects that are too small
    or too big

## Example

``` r
library(clasifierrr)
library(EBImage)
library(ranger)
## basic example code
## 
## 
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

params_df
#> # A tibble: 2 x 3
#>   file                              classif related_file                        
#>   <chr>                             <chr>   <chr>                               
#> 1 /home/jspaezp/R/x86_64-redhat-li… sphero… /home/jspaezp/R/x86_64-redhat-linux…
#> 2 /home/jspaezp/R/x86_64-redhat-li… bg      /home/jspaezp/R/x86_64-redhat-linux…
```

### Form of the classifier files

It has to be a file of the same size of the related image, where it is
all black except for the desired classifier section.

Several classifications can be used.

I personally use gimp to generate those, just open the image you want to
classify, draw on a new layer, disable the main layer and export to a
png.

``` r
display(readImageBw(system.file(
            "extdata", "tiny_4T1-shNT-1_layer1.png",
            package = "clasifierrr")), method = "raster")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r

display(readImageBw(system.file(
            "extdata", "tiny_4T1-shNT-1_layer2.png",
            package = "clasifierrr")), method = "raster")
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />

``` r

base_image <- readImageBw(system.file(
            "extdata", "tiny_4T1-shNT-1.png",
            package = "clasifierrr"))
display(base_image, method = "raster")
```

<img src="man/figures/README-unnamed-chunk-3-3.png" width="100%" />

The classifier is based on the concept of features, which is applying
several filters to the image to detect edges and calculate the
consistency with the neighborhood.

you can use as many filters as you want but THE FILTER WIDTHS HAVE TO BE
ODD NUMBERS, also consider that the more filters, the more memmory you
will need.

``` r
features <- calc_features(base_image, filter_widths = c(3,5))
#> Starting to calculate features for image of width 154 and height 205
#> Filters of size: {3,5}
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:EBImage':
#> 
#>     transpose
#> 
#> Took 0.47 secs to calculate the 7 features for 31570 pixels
head(features, 2)
#>   gauss_filt_3 gauss_filt_5 gauss_diff_3  var_filt_3  var_filt_5 sobel_filt_3
#> 1    0.4868591    0.4882913  0.002864240 0.002066252 0.003918570   0.11379308
#> 2    0.4860857    0.4887673  0.005363217 0.001608498 0.005823959   0.09180157
#>   sobel_filt_5
#> 1    0.1305947
#> 2    0.3050769
```

Each of the columns can be made to an image

``` r
for (i in names(features)) {
    reconstructed_image <- Image(features[[i]], dim(base_image))
    display(reconstructed_image, method = "raster")
    title(main = list(i, col = "yellow"))
}
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-4.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-5.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-6.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-7.png" width="100%" />

``` r
trainset <- build_train_multi(params_df, filter_widths = c(3,5))
#> Returning for file:  /home/jspaezp/R/x86_64-redhat-linux-gnu-library/3.6/clasifierrr/extdata/tiny_4T1-shNT-1_layer1.png and classification" spheroid " a total of { 8556 } positive pixels
#> Returning for file:  /home/jspaezp/R/x86_64-redhat-linux-gnu-library/3.6/clasifierrr/extdata/tiny_4T1-shNT-1_layer2.png and classification" bg " a total of { 14056 } positive pixels
#> Starting to calculate features for image of width 154 and height 205
#> Filters of size: {3,5}
#> 
#> Took 0.16 secs to calculate the 7 features for 31570 pixels
#> Warning in build_train(feat_img = calc_features(preprocess_fun_img(readImageBw(.x)), : The selected train size(50000) is larger than the number of classified pixels (22567)  so the number is getting updated to the total number of available pixels
#> Classified objects are of classesbg: 14056 and spheroid: 8511
#> Returning a data frame of 22567 rows and 8 columns
head(trainset)
#>   gauss_filt_3 gauss_filt_5  gauss_diff_3   var_filt_3   var_filt_5
#> 1   0.25783969   0.29436493  0.0730504790 3.497990e-03 3.492963e-02
#> 2   0.04660868   0.04709824  0.0009791228 1.063219e-05 3.487234e-05
#> 3   0.22005382   0.21696351 -0.0061806254 2.975114e-04 2.484828e-03
#> 4   0.05142401   0.05118543 -0.0004771715 1.385982e-05 4.725202e-05
#> 5   0.45634625   0.45400424 -0.0046840166 9.198741e-04 3.192423e-03
#> 6   0.51705904   0.52248562  0.0108531734 1.238080e-03 4.346767e-03
#>   sobel_filt_3 sobel_filt_5 pixel_class
#> 1   0.44547363   1.74150324    spheroid
#> 2   0.01240109   0.04913711    spheroid
#> 3   0.07058824   0.14509804          bg
#> 4   0.01753779   0.03507558    spheroid
#> 5   0.06200544   0.16935547          bg
#> 6   0.02986578   0.45909159          bg
```

``` r
classifier <- ranger(
    pixel_class ~ .,
    data = trainset, 
    num.trees = 100, 
    importance = "impurity", 
    min.node.size = 5, 
    max.depth = 200)
classifier
#> Ranger result
#> 
#> Call:
#>  ranger(pixel_class ~ ., data = trainset, num.trees = 100, importance = "impurity",      min.node.size = 5, max.depth = 200) 
#> 
#> Type:                             Classification 
#> Number of trees:                  100 
#> Sample size:                      22567 
#> Number of independent variables:  7 
#> Mtry:                             2 
#> Target node size:                 5 
#> Variable importance mode:         impurity 
#> Splitrule:                        gini 
#> OOB prediction error:             0.92 %
```

If the classifier was trained using `importance = "impurity"`, you can
ask it to give you the relative importance of the variables used.

``` r
sort(ranger::importance(classifier), decreasing = TRUE)
#> gauss_filt_3   var_filt_5 gauss_filt_5   var_filt_3 sobel_filt_5 sobel_filt_3 
#>    3041.6183    2289.3106    2106.4064    1337.6011     759.6511     719.9215 
#> gauss_diff_3 
#>     311.2514
```

### Using the classifier on an image

Can be used directly on calculated features …

``` r
test_img <- readImageBw(system.file(
        "extdata", "tiny_4T1-shNT-1.png",
        package = "clasifierrr"))

test_feat <- calc_features(test_img, filter_widths = c(3,5))
#> Starting to calculate features for image of width 154 and height 205
#> Filters of size: {3,5}
#> 
#> Took 0.19 secs to calculate the 7 features for 31570 pixels

class_img <- classify_img(
    classifier, 
    feature_frame = test_feat, 
    dims = dim(test_img), 
    class_highlight = "spheroid")
#> Starting classification
#> Took 0.3043 secs to predict the image
display(class_img, method = "raster")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

It can also be used on a raw image …

``` r
class_img <- classify_img(
  classifier, 
  img = test_img, 
  filter_widths = c(3,5))
#> Attempting to calculate features
#> Starting to calculate features for image of width 154 and height 205
#> Filters of size: {3,5}
#> 
#> Took 0.18 secs to calculate the 7 features for 31570 pixels
#> Starting classification
#> Took 0.2963 secs to predict the image
#> Warning in classify_img(classifier, img = test_img, filter_widths = c(3, : Found in the final classification {12426} values more than 1 and {0} values less than 0, This might be undesired in the final image and lead to inconsistencies
display(colorLabels(class_img), method = "raster")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

And as well in a system file

``` r
class_img <- classify_img(
  classifier,
  path = system.file(
    "extdata", "tiny_4T1-shNT-1.png",
    package = "clasifierrr"),
  filter_widths = c(3,5), 
  class_highlight = "spheroid")
#> Attempting to read image from file/home/jspaezp/R/x86_64-redhat-linux-gnu-library/3.6/clasifierrr/extdata/tiny_4T1-shNT-1.png
#> Attempting to calculate features
#> Starting to calculate features for image of width 154 and height 205
#> Filters of size: {3,5}
#> 
#> Took 0.17 secs to calculate the 7 features for 31570 pixels
#> Starting classification
#> Took 0.3144 secs to predict the image

display(class_img, method = "raster")
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

### Cleaning the final image

The final image can be cleaned manually or using `filter_masks`, which
can remove stuff either too big or small.

As a reminder, white regions are considered objects, so if your object
is black, try running something like `img <- 1- img`

``` r
display(dilate(class_img, makeBrush(3, "disc")), method = "raster")
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

``` r
display(colorLabels(bwlabel(class_img)), method = "raster")
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

``` r
filt_class_img <- filter_masks(class_img, min_radius = 30, max_radius = 1000)

display(
    filt_class_img,
    method = "raster")

display(colorLabels(bwlabel(filt_class_img)), method = "raster")
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />

``` r

table(filt_class_img)
#> filt_class_img
#>     0     1 
#> 19162 12408

filt_class_img
#> Image 
#>   colorMode    : Grayscale 
#>   storage.mode : integer 
#>   dim          : 205 154 
#>   frames.total : 1 
#>   frames.render: 1 
#> 
#> imageData(object)[1:5,1:6]
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    0    0    0    0    0    0
#> [2,]    0    0    0    0    0    0
#> [3,]    0    0    0    0    0    0
#> [4,]    0    0    0    0    0    0
#> [5,]    0    0    0    0    0    0
```
