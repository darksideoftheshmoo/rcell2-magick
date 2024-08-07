% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/definitions_magick.R
\name{cellStrips}
\alias{cellStrips}
\title{Wraps cellMagick to make strips, optionally cutting them.}
\usage{
cellStrips(
  cdata,
  paths,
  n_ucids = NULL,
  cut_breaks = 1,
  split_col = "ucid",
  ch = c("BF.out", "yfp.out"),
  sortVar = "t.frame",
  ...
)
}
\arguments{
\item{cdata}{A "cdata" data.frame with the Cell-ID variables (not the cell.data object, \code{cell.data$data}). It can be obtained from \code{get_cell_data} or \code{load_cell_data}.}

\item{paths}{A "paths" data.frame, with paths to the images (i.e. \code{cell.data$images}), and t.frame, position and channel information of each one. It can be obtained from \code{get_cell_data} or \code{load_cell_data}.}

\item{n_ucids}{Will select the first `n_ucids`, by default (NULL) it selects all.}

\item{cut_breaks}{Will split each strip into `cut_breaks` pieces.}

\item{split_col}{Column from cdata used to separate different sets of cells.}

\item{ch}{Name of the CellID channel (BF, BF.out, RFP, etc.). "BF.out" by default, use a vector to select more than one channel simultaneously (images will be stacked).}

\item{sortVar}{Variable name used to sort the rows (after sampling if a \code{seed} was specified). NULL by default, to skip sorting.}

\item{...}{
  Arguments passed on to \code{\link[=magickCell]{magickCell}}
  \describe{
    \item{\code{max_composite_size}}{Maximum size of the final composite image (this resize is applied last) in pixels. 1000 by default.}
    \item{\code{cell_resize}}{Resize string for the individual cell images (\code{NULL} translates to \code{boxSize}x\code{boxSize} by default).}
    \item{\code{boxSize}}{Lateral size of the square box extracted from the original images (in pixels). 50 px by default.}
    \item{\code{n.cells}}{Maximum number of cells to display (integer, set to \code{NULL} to display all cells in \code{cdata}).}
    \item{\code{equalize_images}}{Use magick's function to "equalize" the image when TRUE (FALSE by default). Can be a logical vector, each value applied separately to each channel (recycled to the length of \code{ch}).}
    \item{\code{normalize_images}}{Use magick's function to "normalize" the image when TRUE (FALSE by default). Can be a logical vector, each value applied separately to each channel (recycled to the length of \code{ch}).}
    \item{\code{customize_images}}{Use a custom magick-like function to "customize" the image when TRUE (FALSE by default). Can be a logical vector, each value applied separately to each channel (recycled to the length of \code{ch}).}
    \item{\code{image_customize}}{A custom magick-like function to "customize" the channels specified in \code{customize_images}. Defaults to NULL (disabled).}
    \item{\code{seed}}{Seed value for sampling of cell images, passed to \code{set.seed}. Set to \code{NULL} by default, which disables sampling. Note that the state of the RNG is restored on exit (see \code{on.exit}).}
    \item{\code{.debug}}{Print more messages if TRUE.}
    \item{\code{return_single_imgs}}{If TRUE, return a vector of images instead of a tile.}
    \item{\code{return_ucid_df}}{If TRUE, return is a list of magick images and ucid dataframes.}
    \item{\code{annotation_params}}{Set to NULL to skip annotations, or a named list with values of length one for magick::annotate options (i.e. one or more of the names "color" "background" "size"). Note that size close to zero can be invisible.}
    \item{\code{add_border}}{Add a 1x1 border to the pictures. Useful for stacking/appending.}
    \item{\code{stack_vertical_first}}{Set to TRUE to stack images vertically first (useful when \code{return_single_imgs = T}).}
    \item{\code{return_raw}}{Returns loaded images prematurely (i.e. without any processing other than magick::image_read and magick::image_crop).}
    \item{\code{crop_images}}{Whether to crop images to a box centered on the cell's XY position (TRUE, default), or the full image (FALSE).}
    \item{\code{fill_cdata}}{If TRUE, the time frames in the paths dataframe will be used to generate missing frames in cdata for a given ucid. It is meant to be used with cdata containing one unique ucid.}
    \item{\code{highlight_frames}}{Pass an array of t.frames, and the cells with matching frames will be highlighted with a yellow border yellow. This overrides other highlights.}
  }}

\item{cut_strips}{Use `cut` to split the image series (by index; preserves sortVar order).}
}
\description{
First, `cdata` is split by `split_col` and then images are generated. 
Only the first `n_ucids` in `cdata` are processed.
Then `images` are split with `cut`, which is useful wen strips are too long.
}
\examples{
# Not run
if(F){
strips <- 
  cellStrips(cdata = cdata \%>\% filter(ucid == 20308),
             paths = image.paths,
             ch = c("BF", "BF.out", "YFP.out"), equalize_images=T,
             # n_ucids = 2,
             cut_breaks = 3)
             
# Get one cell
strips$`20308` \%>\%

  # For "cut > 1" 
  image_join() \%>\%
  image_append() \%>\% 
  
  # For rmarkdown inline rendering
  rcell2::magickForKnitr() \%>\% knitr::include_graphics()
}
}
