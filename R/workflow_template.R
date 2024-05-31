#' A function to donwload the latest worflow tempalte in Rmarkdown
#' 
#' Will donwload the .Rmd file to the current working directory.
#' 
#' @param file_name File name for the wokflow template.
#' @param open.template Try using file.edit to open the file in RStudio after getting it.
#' 
#' @export
get_workflow_template_magick <- function(
    file_name = "rcell2.magick_workflow_template.Rmd",
    open.template = T){
  
  if(file.exists(file_name)) stop("get_workflow_template error: file ", file_name, " already exists.")
  
  workflow.file <- system.file(
    "rmarkdown/templates/rmd_template.magick/skeleton/skeleton.Rmd",
    package = "rcell2.magick"
  )
  
  if(file.exists(workflow.file)){
    file.copy(from = workflow.file, to = file_name)
  } else {
    download.file(url = paste0("https://raw.githubusercontent.com/darksideoftheshmoo/rcell2-magick/main/",
                               "inst/rmarkdown/templates/rmd_template.magick/skeleton/skeleton.Rmd"), 
                  destfile = file_name)
  }
  
  if(open.template){
    file.edit(file_name)
  }
  
}
