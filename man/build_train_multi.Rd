% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow.R
\name{build_train_multi}
\alias{build_train_multi}
\title{Calculates a data frame to train classifiers}
\usage{
build_train_multi(
  imgs_df,
  train_size_each = 50000,
  preprocess_fun_img = NULL,
  preprocess_fun_mask = NULL,
  filter_widths = c(3, 5, 15, 31),
  shape_sizes = c(51, 101, 151, 201, 251)
)
}
\arguments{
\item{imgs_df}{a data frame specifying the masks and images to use, see details}

\item{train_size_each}{number of elements to keep for each mask/image combination}

\item{preprocess_fun_img}{optional, processing function to be applied to each image}

\item{preprocess_fun_mask}{optional, processing function to be applied to each mask}

\item{filter_widths}{numeric vector with the filter widths to be used}
}
\value{
data.frame
}
\description{
Calculates a data frame to train classifiers
}
\examples{
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
    params_df,
    train_size_each = 5000,
    filter_widths = c(3,5), shape_sizes = c(21, 51))
}
