.onLoad <- function(libname, pkgname){
  if(requireNamespace("magick", quietly = T)){
    tryCatch(
      {
        i <- magick::image_read("logo:")
        a <- magick::image_annotate(i, "foo!")
        message("ImageMagick is functional. Happy shinying!")
      }, 
      error = function(e){
        warning("ImageMagick annotations don't seem to work. Consider re-installing the magick package from source, which requires the ImageMagick library to be installed in yout system. For details see: https://github.com/ropensci/magick#installation")
      }
    )
  } else {
    warning("The magick package was not found. Install it by running: install.packages('magick')")
  }
}
# .onAttach <- .onLoad
