# 2D convolution-based linear filter for images and matrix data

# Copyright (c) 2007-2015, Andrzej Oleś, Gregoire Pau, Oleg Sklyar
# Modified 2020 J Sebastian Paez

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


#' @describeIn filter2.prepared precomputes image to handle borders
prep_filter.img <- function(x, filter_dims,
                            boundary = c("circular", "replicate"),
                            val = NULL) {
  if ( is.numeric(boundary) ) {
    val = boundary
    boundary = "linear"
  } else {
    boundary = match.arg(boundary)
  }

  validObject(x)

  # d remains unmodified whilst dx changes
  d = dx = dim(x)

  dnames = dimnames(x)

  # cf stands for center filter ...
  cf = filter_dims %/% 2
  df <- filter_dims

  switch(
    boundary,
    ## default mode just wraps around edges
    circular = {
      x = EBImage::imageData(x)
    },
    ## pad with a given value
    linear = {
      dx[1:2] = dx[1:2] + cf[1:2]

      if ( length(dx) > 2 && length(val) == prod(dx[-(1:2)]) ) {
        # Higher dim array with matching linear boundry values
        xpad = array(rep(val, each = prod(dx[1:2])), dim = dx)
      } else {
        if ( length(val) > 1 ) {
          warning(
            'The boundary value length does not match the number',
            ' of frames, only the first element of boundary will be used')
        }
        xpad = array(val[1], dx)
      }
      # The do.call and arguments as a list of dimensions is to take
      # are of arrays of unknown dimension
      x = do.call(
        "[<-", c(quote(xpad),
                 lapply(d, function(x) enquote(1:x)), quote(x)) )
    },
    replicate = {
      x = EBImage::imageData(x)

      dx[1:2] = dx[1:2] + df[1:2] - 1L

      rep.dim <- function(x, dim, index, times) {
        xs <- abind::asub(x, idx = index, dims = dim, drop = FALSE)
        ds <- dim(xs)
        ds[dim] <- times
        if ( dim == 2L ) {
          xs <- split(xs, ceiling(seq_along(xs)/ds[1L]))
          xs <- sapply(xs, FUN = rep, times = times)
        } else {
          xs <- rep(xs, each = times)
        }
        array(xs, dim = ds)
      }

      # Add left and right colums
      lc <- rep.dim(x, 2L, 1L, cf[2L])
      rc <- rep.dim(x, 2L, d[2L], cf[2L])
      x <- abind::abind(x, rc, lc, along = 2L)
      # Add top and bottom rows
      tr <- rep.dim(x, 1L, 1L, cf[1L])
      br <- rep.dim(x, 1L, d[1L], cf[1L])
      x <- abind::abind(x, br, tr, along = 1L)
    }
  )

  attr(x, "d") <- d
  attr(x, "dx") <- dx
  attr(x, "dnames") <- dnames

  return(x)
}

#' @describeIn filter2.prepared precomputes the filter to be applied
prep_filter.filter <- function(filter, dim_x_proc) {
  # in retrospect ... having this one separate is handy for functional
  # programming and job dispatchment but not really time efficient per-se
  dx <- dim_x_proc

  validObject(filter)

  df = dim(filter)

  ## do not enforce odd filter dimensions in the special case when filter size
  ## equals image size
  if ( any(dx[1:2] != df) ) {
    if (any(df %% 2 == 0)) {
      stop("dimensions of 'filter' matrix must be odd")
    }

    if (any(dx[1:2] < df)) {
      stop("'filter' dimensions cannot exceed dimensions of 'x'")
    }
  }

  # cf stands for center filter ...
  cf = df %/% 2
  of = df %% 2

  ## create fft filter matrix
  wf = matrix(0.0, nrow = dx[1L], ncol = dx[2L])

  wf[c(if (cf[1L] > 0L) (dx[1L] - cf[1L] + 1L):dx[1L] else NULL,
       1L:(cf[1L] + of[1L])),
     c(if (cf[2L] > 0L) (dx[2L] - cf[2L] + 1L):dx[2L] else NULL,
       1L:(cf[2L] + of[2L]))] <- filter

  wf = fftwtools::fftw2d(wf)
  pdx = prod(dx[1:2])

  .filter = function(xx) {
    if (!is.complex(xx)) {
      xx <- fftwtools::fftw2d(xx)
    }

    Re(fftwtools::fftw2d(xx*wf, inverse = 1)/pdx)
  }

  return(.filter)
  # returns a function ...
  # y = .filter(x_image)
  # or ...
  # fft_x <- fftwtools::fftw2d(x_image)
  # y <- .filter(fft_x)
}


#' Calculate spatial filtering on precomputed objects
#'
#' @param prep_x a prepared image file
#' @param raw_x non-prepared image file, defaults to raw_x
#' @param fft_x  output of the prepared image in `fftwtools::fftw2d`
#' @param dims_x dimensions output from the image processing
#' @param dims_x_orig dimension of the original image file
#' @param prep_filter prepared filtering function, output of `prep_filter.filter`
#' @param x_dimnames dimension names of the original image file
#' @param boundary any of "circular", "replicate", or a numeric value to use
#'
#' @return
#' @export
#'
#' @examples
#' f = system.file("images", "sample.png", package="EBImage")
#' img = EBImage::readImage(f)
#' my_filter <- EBImage::makeBrush(31, "gaussian", sigma = 4)
#'
#' prepd_filt <- prep_filter.filter(filter = my_filter, dim_x = dim(img))
#' fft_img <- fftwtools::fftw2d(img)
#'
#' # Image preprocessing can be skiped if using circular to handle boundries...
#' filtered_img <- filter2.prepared(
#'   prep_x = img,
#'   fft_x = fft_img,
#'   dims_x = dim(img),
#'   dims_x_orig = dim(img),
#'   x_dimnames = dimnames(img),
#'   prep_filter = prepd_filt)
#'
#' oldschool_filtered <- EBImage::filter2(img, my_filter)
#' all(oldschool_filtered == filtered_img)
#' # [1] TRUE
filter2.prepared <- function(prep_x,
                             raw_x = prep_x,
                             fft_x = fftwtools::fftw2d(prep_x),
                             dims_x,
                             dims_x_orig,
                             prep_filter,
                             x_dimnames = NULL, # not REALLY used
                             boundary = c("circular", "replicate")) {

  boundary = match.arg(boundary)
  # Circular boundry does not require filter size in the preparation of
  # x ... could be the most efficient way to calculate all of those ...
  res <- raw_x
  attr(res, "d") <- NULL
  attr(res, "dx") <- NULL
  attr(res, "dimnames") <- NULL

  ## convert to a frame-based 3D array
  if ( length(dims_x) > 2L ) {
    y = apply(fft_x, 3:length(dims_x), prep_filter)
    dim(y) = dims_x
  }
  else {
    y = prep_filter(fft_x)
  }

  if ( boundary != "circular" ) {
    y = abind::asub(y, list(1:dims_x_orig[1L], 1:dims_x_orig[2L]), 1:2)
  }

  dimnames(y) = x_dimnames

  EBImage::imageData(res) <- y
  res
}


# for circular the prepd image is the same as the original....
filter2_circular <- function(img, filter, img_fft = fftwtools::fftw2d(img)) {
  prepd_filter <- prep_filter.filter(filter = filter, dim_x_proc = dim(img))
  filter2.prepared(prep_x = img,
                   raw_x = img,
                   fft_x = img_fft,
                   dims_x = dim(img),
                   dims_x_orig = dim(img),
                   prep_filter = prepd_filter,
                   x_dimnames = dimnames(img))
}


