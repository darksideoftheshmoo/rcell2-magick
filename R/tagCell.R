# Added to remove NOTES from devtools:check()
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# globalVariables(strsplit("channel choice counts h i level pos saved treatment w x y", " ")[[1]])
# "A horrible hack" (?)

# I have found the cause of re-rendering plots.
# clientData$output_scatterplot_width changes 10 PIXELS for no reason and triggers a re-render.
# Maybe its the scrollbar or something...

#' Filtrar cdata usando gráficos y dibujando regiones
#'
#' @inheritParams magickCell
#' @inheritParams shinyCell
#' @param cell_tags list of named vectors corresponding to tag groups and tags: list(named_item1 = c(option1, option2, ...), named_item2 ...).
#' @param randomize_ucids Randomize ucid order.
#' @param tag_box_size Size in pixels of the square crop of the image, around the center of the cell. A larger value is useful for larger or more elongated cells.
#' @param cell_resize Resize the main image to this value in pixels (integer). NULL by default, it will fill the available horizontal space.
#' @param tag_channels_select a vector giving names for the image channels: c("BF", "YFP.out", etc....).
#' @param equalize_images Use magick's function to "equalize" the images.
#' @param normalize_images Use magick's function to "normalize" the images.
#' @param seed seed for random sampling of images.
#' @param tmp_output_file File path into which tagging information will be dumped by user request. NULL by default, to automatically create and append to a tmp file.
#' @param tag_ggplot A ggplot created with no data (i.e. only "aes" and a "geom"). It will be passed cdata from the current cell and displayed below. If the x-axis is "t.frame", clicking the plot will jump to the time point nearest to the click.
#' @param tag_ggplot_vars Alternative specification of the \code{tag_ggplot} object. Pass a character vector of length two, holding the names of the two variables in \code{cdata} you want to plot.
#' @param max.frames Max number of t.frames to render in the cell strip. Set to 0 to disable.
#' @param tags.df Previous tag dataframe, used to restore or view previous tags in the app (restores tags that are named in the cell_tags list).
#' @param verbose Print debugging messages (with levels at either 0, 1 or 2).
# @param n_max max number of boxes in the image.
# @param ... extra arguments, not used.
#' @return Lots of stuff.
#' @examples
#' path <- "/mac/apesta/trololololol/"
#' 
#' cell.data <- rcell2::cell.load.alt(path = path)
#' 
#' image.paths <- cell.data$d.paths  # Si usaste load_cell es: image.paths <- rcell2::magickPaths(cell.data)
#' 
#' pdata <- read_tsv(paste0(path, "pdata.csv"))
#' 
#' cdata <- left_join(cell.data$d, pdata)
#' 
#' p <- ggplot() + 
#'   geom_line(aes(x=t.frame, y=cf.y, group=ucid))
#' 
#' tag_channels_select <- c("BF", "BF.out", "YFP", "YFP.out")
#' 
#' saved <- rcell2::tagCell(cdata,
#'                          pdata, 
#'                          image.paths,
#'                          cell_tags = list(far1_drop = c(TRUE,
#'                                                         FALSE),
#'                                           budding =   c("emergence",
#'                                                         "division", 
#'                                                         "shmoo_o_algo"),
#'                                           artifact =  c("segmentation",
#'                                                         "crowding",
#'                                                         "out_of_focus",
#'                                                         "interesante",
#'                                                         "death",
#'                                                         "flown_away",
#'                                                         "not_a_cell")
#'                          ),
#'                          tag_channels_select = tag_channels_select,
#'                          equalize_images = T,
#'                          normalize_images = F,
#'                          n_max = 50,
#'                          tag_box_size = 75,
#'                          cell_resize = 300,
#'                          tag_ggplot = p,
#'                          tmp_output_file = "../output/annotations/progress.csv", 
#'                          debug_messages = F
#'                          )
#'                          
#' @import shiny ggplot2 magick keys
#' @importFrom grDevices rgb
#' @importFrom utils head
#' @export
tagCell <- function(cdata,
                    pdata,
                    paths,
                    cell_tags,
                    randomize_ucids = FALSE,
                    tag_box_size = 60,
                    cell_resize=NULL,
                    tag_channels_select=c("BF", "BF.out"),
                    # n_max=10,
                    seed = 1,
                    tmp_output_file=NULL,
                    tag_ggplot = NULL,
                    tag_ggplot_vars = c("x"="t.frame", "y"="a.tot", "geom"="point"),
                    equalize_images = F,
                    normalize_images = F,
                    # prev.annot.df=NULL,  # TO-DO: implement resume annotations
                    max.frames=10,
                    tags.df=NULL,
                    verbose=0
                    ){
  
  # Check tags.df  
  if(!is.null(tags.df)){
    if(!all(names(cell_tags) %in% names(tags.df))){
      # Get missing columns
      tags_df_missing_cols <- setdiff(names(cell_tags),
                                      names(tags.df))
      
      # Warn
      warning(paste0(
        "tagCell: some names in the 'cell_tags' were missing from the provided 'tags.df', adding NA columns for names: '",
        paste(tags_df_missing_cols, collapse = "', '"),
        "'."
      ))
      
      # Fill them with NAs
      tags.df[, tags_df_missing_cols] <- NA
    }
  }
  
  # Hotkeys (for the keys package)
  hotkeys <- c(
    "left", 
    "right",
    "shift+left", 
    "shift+right"
  )
  
  # Debug message level
  if(!all(verbose %in% 0:2)) stop("tagCell: error, verbose level must be 0, 1 or 2.")
  switch (verbose+1,
          {
            runtime_messages <- F
            debug_messages <- F
          },
          {
            runtime_messages <- T
            debug_messages <- F
          },
          {
            runtime_messages <- T
            debug_messages <- T
          }
  )
  
  # Check NAs in ucid variable
  if(any(is.na(cdata[["ucid"]]))) stop("\ntagCell: ucid variable contains NA values")
  
  # Check ucid type and convert to integer
  ucid_class_check <- class(cdata[["ucid"]])
  if(ucid_class_check != "integer"){
    
    if(ucid_class_check == "factor"){
      warning(paste("\ntagCell: cohercing factor ucid to integer type"))
      cdata <- dplyr::mutate(cdata, ucid = as.integer(as.character.factor(ucid)))
      
    } else {
      warning(paste("\ntagCell: cohercing", ucid_class_check, "ucid to integer type"))
      cdata <- mutate(cdata, ucid = as.integer(ucid))
    }
  }
  
  # Progress file
  if(is.null(tmp_output_file)){
    tmp_output_file <- tempfile(tmpdir = tempdir(), fileext = ".txt", pattern = "tagCell_progress")
  } else {
    dir.create(dirname(normalizePath(tmp_output_file)), recursive = T, showWarnings = F)
  }
  if(debug_messages) print(paste("Will append tagging progress to file:", tmp_output_file))

    
  # Setup environments for the shiny app, from this environment
  environment(tagCellServer) <- environment()
  environment(tagCellUi) <- environment()
  
  #### RUN APP ####
  tags.df <- shiny::runApp(list(ui = tagCellUi(), server = tagCellServer))
  
  # Check tags.df (again)
  if(!all(names(cell_tags) %in% names(tags.df))){
    # Get missing columns
    tags_df_missing_cols <- setdiff(names(cell_tags),
                                    names(tags.df))
    
    # Warn
    message(paste0(
      "tagCell: some names in the 'cell_tags' were not used, adding NA columns to 'tags.df' for names: '",
      paste(tags_df_missing_cols, collapse = "', '"),
      "'."
    ))
    
    # Fill them with NAs
    tags.df[, tags_df_missing_cols] <- NA
  }
  
  #### RETURN RESULT ####
  return(tags.df)
}
            
#' Pivot cell tags to a cdata-joinable dataframe, with one hot encoding
#' 
#' @param tags.df Output from tagCell.
#' @param exclude.cols Character vector with names of columns which should be removed from input.
#' @importFrom data.table melt setDT dcast
#' @details 
#' Ver:
#' * `~/Projects/Academia/Doctorado/gitlabs_acl/rtcc/far1/analisis_Far1_arresto-lavado/R/analisis_pos_2_a_7_v7_tags_analysis.Rmd`
#' * https://stackoverflow.com/questions/55288338/r-multi-hot-encoding-among-multiple-columns
#' * https://stackoverflow.com/a/63454411/11524079
#' @export
tags.to.onehot <- function(tags.df, exclude.cols = c("pos", "cellID", "viewed")){
  annotations.dt <- tags.df %>% filter(!is.na(t.frame)) %>% 
    # Remove some redundant ID columns
    {.[,!names(.) %in% exclude.cols]}
  
  # Replace boolean values with strings
  # annotations.dt <- annotations #%>%
    # mutate(far1_drop = ifelse(far1_drop, "t_drop", "false_drop")) %>%
    # mutate(far1_deloc = ifelse(far1_deloc, "far1_deloc", "far1_no_deloc"))
  
  # Replace missing values with a string
  # annotations.dt[is.na(annotations.dt)] <- "not_tagged"
  
  # Convert all columns to factor type
  annotations.dt <- dplyr::mutate_all(annotations.dt, factor)
  
  # Set data table
  data.table::setDT(annotations.dt)
  
  # Melt data table by ucid and t.frame
  annotations.dt <- annotations.dt %>% data.table::melt(id.vars = c("ucid","t.frame"))
  
  # Paste variable names with their values.
  # These end up as column names after casting (see below).
  annotations.dt <- annotations.dt %>% 
    unite(value, variable, value, sep = ".") %>% 
    mutate_all(factor)
  
  # Esta funcion va a calcular el valor de una celda solo cuando
  # hay múltiples valores para ella en la tabla original (esos vienen de columna "ind", ver más abajo).
  # Se usa entonces cuando las filas no son identificadas únicamente por las columnas de ID (aunque quizás se use siempre :shrug:).
  aggr.fun <- function(x) length(x) > 0
  
  annotations.dt <- data.table::dcast(
    # Add "ind" column, filled with ones.
    data.table::setDT(annotations.dt)[,ind:=1],  # [1,],  # [c(1,1,2,3),],
    # ?
    fun.aggregate	= aggr.fun,
    # Specify what columns are identifiers (LHS),
    # and which columns have values that will be cast to columns (RHS).
    ucid+t.frame~value,
    # The column holding the values that should be cast
    value.var = "ind",
    # The fill value for missing values for a combination if ID columns.
    fill=FALSE
  ) %>%
    # Convert ucid and t.frame factors to characters
    mutate(ucid = as.character.factor(ucid),
           t.frame = as.character.factor(t.frame)) %>%
    # Then convert everything to numeric type
    mutate_all(as.integer)
  
  return(annotations.dt)
}
