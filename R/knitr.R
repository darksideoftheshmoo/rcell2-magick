
#' Display an image in rmarkdown with knitr
#'
#' @param imgs a magick image or cellMagick output
#' @param .prefix A string prepended to the file name, "" by default.
#' @param .resize a cellMagick image resize string as "200x200" (default NULL, for no resizing).
#' @param .path Directory where the output should be saved.
#' @param .file Full path to the file name where the image will be saved. Overrides all other file name parameters.
# @param .include Set to true to use knitr::include_graphics directly.
#' @param .print Print the image path.
#' @inheritParams magick::image_write
#' @inheritDotParams magick::image_write
#' @return An path to a temporary image file.
# @examples
# cell.args <- cellArgs(path = path)
#' @export
magickForKnitr <- function(imgs, 
                           format = "png",
                           .prefix = "tile", 
                           .resize = NULL, 
                           .path = tempdir(),
                           .file = NULL,
                           .print=F,
                           ...){
  
  dir.create(.path, recursive = T, showWarnings = F)
  
  if (is.null(.file)) {
    temp <- tempfile(tmpdir = .path, fileext = paste0(".", format), pattern = .prefix)
  } else {
    temp <- .file
  }
  
  if(class(imgs) == "list") imgs <- imgs$img
  
  imgs %>% 
    {if(is.null(.resize)) . else magick::image_resize(., .resize)} %>% 
    magick::image_write(path = temp, format = format, ...)
  
  if(.print) print(temp)
  
  
  # if(.include){
  #   include.env <- new.env()
  #   include.env$temp <- temp
  #   eval(expr=parse(text="knitr::include_graphics(temp)"), envir = list(parent.env(env = environment()), include.env))
  # }
  
  return(temp)
}
