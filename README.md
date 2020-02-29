
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clasifierrr

<!-- badges: start -->

<!-- badges: end -->

## Motivation

The goal of clasifierrr is to … classify image regions using R.

In my field of study, there are many cases where one has a lot of images
that require classification. Whether it is counting cells, quantifying
areas in IHC, counting dots in cells, measuring spheroid sizes … And it
would be nice to automate those tasks.

Nonetheless, many of those classification parameters are
experiment-specific, making a generalized classifier inpractical. In
addition, the number of images is usually enough to be extremely tedious
to work out by a human but not enough to train a neural network.

clasifierrr takes care of those cases, where small datasets are used to
build a clasification algorithm and allow the user to deploy it on many
images.

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
            "extdata", "4T1-shNT-1_layer1.png",
            package = "clasifierrr"),
        system.file(
            "extdata", "4T1-shNT-1_layer2.png",
            package = "clasifierrr")),
    classif = c("spheroid", "bg"),
    related_file = system.file(
        "extdata", "4T1-shNT-1.png",
        package = "clasifierrr")
)

params_df
#> # A tibble: 2 x 3
#>   file                              classif  related_file                       
#>   <chr>                             <chr>    <chr>                              
#> 1 /tmp/RtmpskBOn7/temp_libpathe09c… spheroid /tmp/RtmpskBOn7/temp_libpathe09c53…
#> 2 /tmp/RtmpskBOn7/temp_libpathe09c… bg       /tmp/RtmpskBOn7/temp_libpathe09c53…
```

### Form of the classifier files

It has to be a file of the same size of the related image, where it is
all black except for the desired classifier section.

Several classifications can be used.

I personally use gimp to generate those, just open the image you want to
classify, draw on a new layer, disable the main layer and export to a
png.

``` r
base_image <- readImageBw(params_df[[3]][[1]])
display(
  EBImage::combine(
    base_image,
    readImageBw(params_df[[1]][[1]]),
    readImageBw(params_df[[1]][[2]])),
  method = "raster", all = TRUE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Calculating Features

The classifier is based on the concept of features, which is applying
several filters to the image to detect edges and calculate the
consistency with the neighborhood.

you can use as many filters as you want but THE FILTER WIDTHS HAVE TO BE
ODD NUMBERS, also consider that the more filters, the more memmory you
will need.

``` r
features <- calc_features(
  base_image, filter_widths = c(3,5),
  shape_sizes = c(201, 501))
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:EBImage':
#> 
#>     transpose
head(features, 2)
#> # A tibble: 2 x 10
#>   gauss_filt_3 gauss_filt_5 DoG_filt_3 DoG_filt_5 var_filt_3 var_filt_5
#>          <dbl>        <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#> 1        0.478        0.451   -3.76e-9    -0.0275    0.00409    0.00418
#> 2        0.478        0.457   -3.05e-9    -0.0212    0.00295    0.00347
#> # … with 4 more variables: sobel_filt_3 <dbl>, sobel_filt_5 <dbl>,
#> #   c_hough_trans_201 <dbl>, c_hough_trans_501 <dbl>
```

Each of the columns can be made to an image

``` r
display_filters(features, dims = dim(base_image), scale = TRUE)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### Building a training dataset from the files

The program goes in to the files, generates the features form the base
image and assigns a classification to the pixles that overlap with the
region you defined in the “classifier files” (the ones that look like
squigly lines)

``` r
trainset <- build_train_multi(
  params_df, 
  filter_widths = c(3,5),
  shape_sizes = c(201, 501))
#> Returning for file:  /tmp/RtmpskBOn7/temp_libpathe09c5353558e/clasifierrr/extdata/4T1-shNT-1_layer1.png and classification" spheroid " a total of { 108470 } positive pixels
#> Returning for file:  /tmp/RtmpskBOn7/temp_libpathe09c5353558e/clasifierrr/extdata/4T1-shNT-1_layer2.png and classification" bg " a total of { 205401 } positive pixels
#> Classified objects are of classes {bg: 32737} and {spheroid: 17263}
#> Returning a data frame of 50000 rows and 11 columns
head(trainset)
#>   gauss_filt_3 gauss_filt_5    DoG_filt_3    DoG_filt_5    var_filt_3
#> 1   0.54901961   0.54756148 -2.389017e-10 -1.458124e-03  3.499762e-05
#> 2   0.04313725   0.04313725 -5.965905e-18 -9.107298e-18 -1.084202e-19
#> 3   0.04313726   0.04416861  1.791762e-10  1.031358e-03  2.510378e-06
#> 4   0.05098039   0.05210003  1.791762e-10  1.119641e-03  3.206533e-06
#> 5   0.52156863   0.51723076 -6.569796e-10 -4.337863e-03  2.677033e-05
#> 6   0.52156863   0.52137713 -5.972548e-11 -1.914968e-04  4.103096e-05
#>      var_filt_5 sobel_filt_3 sobel_filt_5 c_hough_trans_201 c_hough_trans_501
#> 1  7.379485e-05 9.028129e-02 2.923345e-01     -2.224892e-17      0.0638463874
#> 2 -4.607859e-19 6.305576e-16 1.491817e-16      7.401487e-17      0.9950793330
#> 3  3.535391e-06 1.240109e-02 1.240109e-02     -1.850372e-17      0.9181997043
#> 4  3.083711e-06 1.240109e-02 2.630668e-02     -5.551115e-17      0.9977565476
#> 5  5.778346e-05 4.960436e-02 2.850632e-01     -3.688827e-17      0.0007445788
#> 6  1.044161e-04 3.230478e-01 1.090203e+00      2.814550e-17      0.1617958842
#>   pixel_class
#> 1          bg
#> 2    spheroid
#> 3    spheroid
#> 4    spheroid
#> 5          bg
#> 6          bg
```

### Train a classifier

Here we use any “machine learning” algorithm as our classifier. I really
like ranger, so that is what I will recommend.

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
#> Sample size:                      50000 
#> Number of independent variables:  10 
#> Mtry:                             3 
#> Target node size:                 5 
#> Variable importance mode:         impurity 
#> Splitrule:                        gini 
#> OOB prediction error:             0.34 %
```

If the classifier was trained using `importance = "impurity"`, you can
ask it to give you the relative importance of the variables used.

``` r
sort(ranger::importance(classifier), decreasing = TRUE)
#> c_hough_trans_501      gauss_filt_3      gauss_filt_5        var_filt_5 
#>        6397.61367        5038.73699        4324.03584        2931.67772 
#>        var_filt_3      sobel_filt_5      sobel_filt_3 c_hough_trans_201 
#>        1586.17721        1069.13067         921.71166         107.82426 
#>        DoG_filt_3        DoG_filt_5 
#>          96.61463          86.87910
```

### Using the classifier on an image

The `classify_img` has an interface for many kinds of inputs depending
on what you have planned.

It Can be used directly on calculated features …

``` r
# This just reads the image to classify
test_img <- readImageBw(system.file(
        "extdata", "4T1-shNT-1.png",
        package = "clasifierrr"))

test_feat <- calc_features(
  test_img, filter_widths = c(3,5),
  shape_sizes = c(201, 501))

class_img <- classify_img(
    classifier, 
    feature_frame = test_feat, 
    dims = dim(test_img), 
    filter_widths = c(3,5),
    shape_sizes = c(201, 501),
    class_highlight = "spheroid")
#> Starting classification
#> Took  10.45   secs  to predict the image
display(class_img, method = "raster")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

It can also be used on a raw image …

``` r
class_img <- classify_img(
  classifier, 
  img = test_img, 
  filter_widths = c(3,5), 
  shape_sizes = c(201, 501))
#> Attempting to calculate features
#> Starting classification
#> Took  9.574   secs  to predict the image
#> Warning in highlight_category(pred_mat, class_highlight): Found in the final classification {307737} values more than 1 and {0} values less than 0, This might be undesired in the final image and lead to inconsistencies
# display(colorLabels(class_img), method = "raster")
```

And as well in a system file

``` r
class_img <- classify_img(
  classifier,
  path = system.file(
    "extdata", "4T1-shNT-1.png",
    package = "clasifierrr"),
  filter_widths = c(3,5), 
  shape_sizes = c(201, 501),
  class_highlight = "spheroid")
#> Attempting to read image from file: /tmp/RtmpskBOn7/temp_libpathe09c5353558e/clasifierrr/extdata/4T1-shNT-1.png
#> Attempting to calculate features
#> Starting classification
#> Took  10.61   secs  to predict the image

# display(class_img, method = "raster")
```

### Cleaning the final image

The final image can be cleaned manually or using `filter_masks`, which
can remove stuff either too big or small.

As a reminder, white regions are considered objects, so if your object
is black, try running something like `img <- 1- img`

``` r
filt_class_img <- filter_masks(min_radius = 10, max_radius = Inf, mask = class_img)

display(filt_class_img, method = "raster")
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

``` r
table(filt_class_img)
#> filt_class_img
#>      0      1 
#> 480266 306166
filt_class_img
#> Image 
#>   colorMode    : Grayscale 
#>   storage.mode : integer 
#>   dim          : 1024 768 
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
