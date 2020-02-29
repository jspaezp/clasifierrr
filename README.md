
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

Feel free to take a look at the documentation in the form of a website
at
![jspaezp.github.io/clasifierrr/](https://jspaezp.github.io/clasifierrr/)

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
#> 1 /tmp/RtmpKz2FgQ/temp_libpath25f4… spheroid /tmp/RtmpKz2FgQ/temp_libpath25f46d…
#> 2 /tmp/RtmpKz2FgQ/temp_libpath25f4… bg       /tmp/RtmpKz2FgQ/temp_libpath25f46d…
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
#> Returning for file:  /tmp/RtmpKz2FgQ/temp_libpath25f46de7b226/clasifierrr/extdata/4T1-shNT-1_layer1.png and classification" spheroid " a total of { 108470 } positive pixels
#> Returning for file:  /tmp/RtmpKz2FgQ/temp_libpath25f46de7b226/clasifierrr/extdata/4T1-shNT-1_layer2.png and classification" bg " a total of { 205401 } positive pixels
#> Classified objects are of classes {bg: 32591} and {spheroid: 17409}
#> Returning a data frame of 50000 rows and 11 columns
head(trainset)
#>   gauss_filt_3 gauss_filt_5    DoG_filt_3    DoG_filt_5   var_filt_3
#> 1   0.28627451   0.28094544 -7.764304e-10 -0.0053290695 2.905077e-04
#> 2   0.59215686   0.59071082 -2.389017e-10 -0.0014460387 2.921742e-05
#> 3   0.48235294   0.48320359  1.194508e-10  0.0008506473 4.830895e-05
#> 4   0.04313725   0.04351016  5.972540e-11  0.0003729058 1.750936e-06
#> 5   0.59607843   0.59372736 -3.583525e-10 -0.0023510696 7.324396e-05
#> 6   0.65882353   0.65937322  5.972532e-11  0.0005496918 4.185369e-05
#>     var_filt_5 sobel_filt_3 sobel_filt_5 c_hough_trans_201 c_hough_trans_501
#> 1 8.137227e-04   0.40042269  1.069546286     -6.735380e-17      1.176725e-01
#> 2 1.176958e-04   0.04471276  0.248052778      7.896517e-17      6.316173e-04
#> 3 9.448909e-05   0.11008368  0.454445342      4.627803e-17      1.096233e-01
#> 4 1.922960e-06   0.01240109  0.005545936      6.155310e-03      9.999985e-01
#> 5 1.765138e-04   0.09444388  0.344406616      6.778361e-17     -2.793893e-17
#> 6 9.542400e-05   0.08680762  0.248331611      3.521661e-01      3.577437e-02
#>   pixel_class
#> 1    spheroid
#> 2          bg
#> 3          bg
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
#> OOB prediction error:             0.39 %
```

If the classifier was trained using `importance = "impurity"`, you can
ask it to give you the relative importance of the variables used.

``` r
sort(ranger::importance(classifier), decreasing = TRUE)
#> c_hough_trans_501      gauss_filt_3      gauss_filt_5        var_filt_5 
#>        8305.93737        4801.83477        3832.46816        2447.06453 
#>        var_filt_3      sobel_filt_5      sobel_filt_3 c_hough_trans_201 
#>        1861.87359         671.06012         438.10366         128.73146 
#>        DoG_filt_5        DoG_filt_3 
#>          99.04194          66.01028
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
#> Took  8.423   secs  to predict the image
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
#> Took  8.445   secs  to predict the image
#> Warning in highlight_category(pred_mat, class_highlight): Found in the final classification {311637} values more than 1 and {0} values less than 0, This might be undesired in the final image and lead to inconsistencies
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
#> Attempting to read image from file: /tmp/RtmpKz2FgQ/temp_libpath25f46de7b226/clasifierrr/extdata/4T1-shNT-1.png
#> Attempting to calculate features
#> Starting classification
#> Took  8.499   secs  to predict the image

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
#> 475968 310464
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
