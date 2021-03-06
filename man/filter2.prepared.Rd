% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter2.R
\name{prep_filter.img}
\alias{prep_filter.img}
\alias{prep_filter.filter}
\alias{filter2.prepared}
\title{Calculate spatial filtering on precomputed objects}
\usage{
prep_filter.img(
  x,
  filter_dims,
  boundary = c("circular", "replicate"),
  val = NULL
)

prep_filter.filter(filter, dim_x_proc)

filter2.prepared(
  prep_x,
  raw_x = prep_x,
  fft_x = fftwtools::fftw2d(prep_x),
  dims_x,
  dims_x_orig,
  prep_filter,
  x_dimnames = NULL,
  boundary = c("circular", "replicate")
)
}
\arguments{
\item{boundary}{any of "circular", "replicate", or a numeric value to use}

\item{prep_x}{a prepared image file}

\item{raw_x}{non-prepared image file, defaults to raw_x}

\item{fft_x}{output of the prepared image in `fftwtools::fftw2d`}

\item{dims_x}{dimensions output from the image processing}

\item{dims_x_orig}{dimension of the original image file}

\item{prep_filter}{prepared filtering function, output of `prep_filter.filter`}

\item{x_dimnames}{dimension names of the original image file}
}
\value{

}
\description{
Calculate spatial filtering on precomputed objects
}
\section{Functions}{
\itemize{
\item \code{prep_filter.img}: precomputes image to handle borders, returns an image
with attributes

\item \code{prep_filter.filter}: precomputes the filter to be applied and returns a function,
requires the dimensions of the post-processed image.
}}

\examples{
f = system.file("images", "sample.png", package="EBImage")
img = EBImage::readImage(f)
my_filter <- EBImage::makeBrush(31, "gaussian", sigma = 4)

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

oldschool_filtered <- EBImage::filter2(img, my_filter)
all(oldschool_filtered == filtered_img)
# [1] TRUE
}
\keyword{internal}
