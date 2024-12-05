# SHINY HELPER FUNCTIONS ####

### DEFINITIONS ###

#' Find cell closest to hover point in shiny
#' @param ui_input the "input" object from whiny server
#' @inheritParams magickCell
#' @keywords internal
hover_closest <- function(ui_input, cdata){
    d.closest <- sqrt(  
        (ui_input$hover$x - cdata[[ui_input$x]])^2 +
            (ui_input$hover$y - cdata[[ui_input$y]])^2
    ) %>% {cdata[which.min(.),]}
    return(d.closest)
}

#' convert sequence of numbers to ranges
#' @importFrom IRanges IRanges
#' @importFrom IRanges reduce
#' @keywords internal
numbers_to_intervals <- function(numbers = c(1:10, 20:30)){
    print("F0.1 numbers_to_intervals")
    x <- IRanges::reduce(IRanges::IRanges(numbers, width = 1))
    x <- as.data.frame(x)[,-3]
    x <- apply(x, 1, function(x) paste(x, collapse = "-"))
    paste(x, collapse = ", ")
}

#' rangeExpand
#' 
#' Function for producing an integer vector, by parsing integer ranges provided as a string: a comma-separated list of integers and "ranges" (i.e. "1:3" or "1-3").
#' 
#' See https://www.rosettacode.org/wiki/Range_expansion#R
#' @param text the input string to parse. Expect unexpected behaviour with decimal numbers, use integers only: \code{text = "1:2, 7-9"}.
#' @param maxPos total amount of positions (an int)
#' @return Integer vector, an expansion of the ranges specified in the input string.
#' @keywords internal
# @export
rangeExpand <- function(text = "1:2, 7-9", maxPos) {
    print("F0.2 rangeExpand")
    text <- gsub("[^0-9,\\:\\-]", replacement = "", text)  # Remove anything that is not integers or separators: ":" "-" ","
    lst <- gsub("(\\d)[-\\:]", "\\1:", unlist(strsplit(text, ",")))
    numeritos <- unlist(sapply(lst, function (x) eval(parse(text=x))), use.names=FALSE)
    # numeritos <- numeritos[numeritos < maxPos]  # better to fail
    if (length(numeritos) > 0) as.numeric(numeritos) else 1:maxPos
}

#' Return Shiny brush vertices
#' @keywords internal
square <- function(x1, y1, x2, y2){
    print("F0.3 square")
    return(list(
        x = c(x1, x1, x2, x2),
        y = c(y1, y2, y2, y1)
    ))
}

#' Check dataframes for NA, NaN or Inf values.
#' 
#' @param df The data.frame to check.
#' @param print.which Print which columns have bad values to the console.
#' @return Logical value: FALSE (check passed) or TRUE (check failed; nasty values found).
#' @keywords internal
has.na_nan_inf <- function (df, print.which = F) {
  r <- lapply(df, function(x) is.nan(x) | is.infinite(x) | 
                is.na(x))
  result <- any(unlist(r))
  
  if(print.which & result){
    r.cols <- unlist(lapply(r, any))
    
    if(result){
      print("Found either NA, NaN or Inf values in the following columns:")
    } else {
      print("Did not find either NA, NaN or Inf values in the input's colums.")
    }
    
    print(r.cols[r.cols])
  }
  
  return(result)
}

# To-do: hacer algo para poder armar filtros fuera de la app. ¿Cómo hacer filtros de una sola variable? El polígono no sirve.
#filterBox <- function(xvar, xmin, xmax, yvar, ymin, ymax)

#' Parse ggplot facet formula and get variables as a character vector
#' 
#' Una función para procesar el facet string y que devuelva las variables presentes en names(pdata) en un character vector
#' 
#' @keywords internal
getFacetVars <- function(pdata, facetFormulaString = "pos ~ treatment"){
    print("F1 getFacetVars")
    facetFormula <- eval(parse(text=facetFormulaString))
    facetVars <- all.vars(facetFormula)
    facetVars <- facetVars[facetVars %in% names(pdata)]
    return(facetVars)
}

#' Get a list of numbers from the "position" input string
#' 
#' @keywords internal
#' 
getPositions <- function(input, numPos){
    print("F2 getPositions")
    positions <- sort(strtoi(strsplit(input, split = ' |,|,,| ,|, ')[[1]]))
    if(length(positions) == 0) positions <- seq.int(from = 1, to = numPos, by = 1)
    return(positions)
}

# POLY FILTER FUNCTIONS ####

#' Apply polygonal filters to cdata
#' 
#' Adds a boolean "filter" column to the "cdata" dataframe, based on a polygon list (typically output by shinyCell).
#' 
#' @param polygon_df_list A list of polygon dataframes with columns: x (values) y (values) xvar (variable name for x values) yvar (variable name for y values) type ("Exclude" or "Include")
#' @param truthMode Priority for "Exclude" and "Include" polygon filter types, passed to \code{\link{calculateTruth}}. Must be either "all" (exclusion overcomes inclusion) or "any" (inclusion overcomes exclusion).
#' @param cell_unique_id_field Name for the column holding the unique identifier (a "primary key") for each data point (i.e. the "ucid" is not suficcient for time series datasets).
#' @inheritParams magickCell
#' 
#' @return a "saved_data" list object, where the cdata is appended a "filter" logical column.
#' 
polyFilterApply <- function(polygon_df_list,
                            cdata,
                            truthMode = "all",
                            cell_unique_id_field = "ucid"){
  
  # Check uniqueness of cell_unique_id_field
  is_primary_key <- length(cdata[,cell_unique_id_field, drop = T]) == length(unique(cdata[,cell_unique_id_field, drop = T]))
  if(!is_primary_key) stop(paste0("Error in polyFilterApply: '", cell_unique_id_field, "' is not a unique identifier."))

  print("F3 polyFilterApply")
  # Initialize empty cfilter, only with a primary key and TRUE filter
  cfilter <- data.frame(id = cdata[,cell_unique_id_field], 
                        filter = TRUE)
  names(cfilter)[1] <- cell_unique_id_field

  # Populate cfilter columns, append one column per filtering polygon
  print("F4.0 polyFilterApply")
  for (i in seq_along(polygon_df_list)) cfilter = do.call(what = polyFilterCell,
                                                          args = list(cdataDF = cdata,
                                                                      filterDF = cfilter,  # Iterates over the output
                                                                      polygonDF = polygon_df_list[[i]],
                                                                      polygonName = paste0("polygon", i)))
  
  # Recalcular la verdad
  print("F6.0 polyFilterApply")
  cfilter <- calculateTruth(filterDF = cfilter, mode = truthMode, 
                            cell_unique_id_field = cell_unique_id_field)

  # Add TRUTH column to cdata
  print("F7.0 polyFilterApply")
  cdata <- applyFilter(cdataDF = cdata, filterDF = cfilter, 
                       cell_unique_id_field = cell_unique_id_field)

  # Return cdata and cfilter in a list.
  return(list(cdata = cdata,
              cfilter = cfilter))
}

#' Build filterDF from polygonDF and cdataDF
#' 
#' @param cdataDF A "cdata" dataframe.
#' @param inclusive_logic_name Logic name for the "inclusive" group of filters, selecting their union / inclusive or. 
#' @param exclusive_logic_name Logic name for the "exclusive" group of filters, selecting the complement of their union. 
#' 
#' @keywords internal
polyFilterCell <- function(cdataDF, filterDF, polygonDF, polygonName = 1, 
                           inclusive_logic_name = "Include",
                           exclusive_logic_name = "Exclude"){
    print(paste("F4: polygon name:", polygonName))
    # polygonDF is NULL

    xvar <- polygonDF[1, "xvar", drop = T]
    yvar <- polygonDF[1, "yvar", drop = T]

    pips <- pip(points = cdataDF[,c(xvar, yvar)], pgn = polygonDF)

    type <- polygonDF[1, "type"]  # Polygon filter type, as chosen at the shiny app

    if(type == inclusive_logic_name) {
        filterDF[, paste0(polygonName, "_", type)] <-  as.logical(pips)

    } else if(type == exclusive_logic_name) {
        filterDF[, paste0(polygonName, "_", type)] <- !as.logical(pips)

    } else {
        print("F4: polygon name: Filter type not within polyFilterCell() options.")
    }

    return(filterDF)
}

#' Polygon filtering function using "sp" package
#' 
#' Decides wether a data point is in a polygon.
#' 
#' @importFrom sp point.in.polygon
#' @keywords internal
pip <- function(points, pgn, points_x_column = 1, points_y_column = 2, pgn_x_column = 1, pgn_y_column = 2){
    print("F5: computing pips")
    # Points dataframe and polygon dataframe, each with only two columns corresponding to x and y values.
    if(nrow(pgn) == 0){
        ret_value <- rep(0,nrow(points))
    } else {
        pips <- sp::point.in.polygon(point.x = points[[points_x_column]],
                                     point.y = points[[points_y_column]],
                                     pol.x = pgn[[pgn_x_column]],
                                     pol.y = pgn[[pgn_y_column]])
        ret_value <- pips
        # Returning an array of 0/1 values, depending on rows fitting or not in the polygon.
    }
    return(ret_value)
}

# REVISAR - tomar en cuenta todos los filtros para decidir si una célula es filtrada por alguno o no.
#' Calculate how filters apply to data according to the logic "mode"
#' 
#' This function is meant to be called exclusively by \code{\link{polyFilterApply}},
#' as it produces appropriate inputs.
#' 
#' @param filterDF The "cfilter" dataframe, as built by \code{\link{polyFilterApply}} by iteratively calling \code{\link{polyFilterCell}}.
#' @param mode Filter logic priority. Must be either "all" (exclusion overrides inclusion) or "any" (inclusion overrides exclusion).
#' @param inclusive_logic_name Logic name for the "inclusive" group of filters, selecting their union / inclusive or. 
#' @param exclusive_logic_name Logic name for the "exclusive" group of filters, selecting the complement of their union. 
#' @param truth_column Name of the column with the boolean filter.
#' 
#' @keywords internal
#' 
calculateTruth <- function(filterDF, 
                           cell_unique_id_field = "ucid", 
                           truth_column = "filter",
                           mode = "all",
                           inclusive_logic_name = "Include",
                           exclusive_logic_name = "Exclude"){
    print("F6.1: calculateTruth")
    # browser()
    # Descartar columnas que no me interesan para calcular la verdad
    drops <- c(cell_unique_id_field, truth_column)
    fDF <- filterDF[, !(names(filterDF) %in% drops), drop = FALSE]

    # Find filter types from the filterDF column names; names come from polyFilterCell()
    types <- sub(".*_(.*)", "\\1", names(filterDF)[!names(filterDF) %in% drops])
    types <- setNames(1:2, c(inclusive_logic_name, exclusive_logic_name))[types]
    additive_types <- which(types == 1)     # identify yes-type columns
    subtractive_types <- which(types == 2)  # identify not-type columns
    
    # Tomar las columnas de filterDF que contienen los valores de verdad para cada filtro
    # y hacerles un all() por fila.
    filterDF$filter <- apply(X = fDF,
                             MARGIN = 1,        # iterate over rows
                             FUN = function(r){ # grab the row and convert it to an array
                                 r <- array(r)  # r <- array(c(T, F))
                                 r[is.na(r)] <- F  # NAs will be converted to FALSE

                                 r_at <- if (length(additive_types) == 0) T else r[additive_types]
                                 r_st <- if (length(subtractive_types) == 0) T else r[subtractive_types]

                                 if(mode == "all"){  # Subtractive overcomes Additive
                                     # all(r)        # Keep only if ALL conditions are true:
                                     all(any(r_at),  # Passes any of the aditive types
                                         all(r_st))  # Passes all of the subtractive types

                                 } else if(mode == "any"){  # Additive overcomes Subtractive
                                     # all(r)        # Keep if ANY condition is true:
                                     any(any(r_at),  # Passes any of the aditive types
                                         all(r_st))  # Passes all of the subtractive types

                                 } else {
                                     print("calculateTruth() mode not in options, defaulting to all()")
                                     all(r)
                                 }
                             })
    print("F6.2 calculateTruth")
    return(filterDF)
}

#' Add TRUTH column to cdata
#' 
#' This function is meant to be called exclusively by \code{\link{polyFilterApply}},
#' as it produces appropriate inputs.
#' 
#' @param filterDF The filtering data.frame as produced by \code{\link{calculateTruth}} in \code{\link{polyFilterApply}}.
#' @param cdataDF The \code{cdata} data.frame.
#' @param truth_column Name of the column in \code{filterDF} with the boolean filter.
#' @param cell_unique_id_field Name for the column holding the unique identifier (a "primary key") for each data point (i.e. the "ucid" is not suficcient for time series datasets).
#' 
#' @keywords internal
applyFilter <- function(cdataDF, filterDF, cell_unique_id_field = "ucid", truth_column = "filter"){
    print("F7.1 applyFilter")
    # Agregar a "cdata" una columna de TRUE/FALSE que refleje si cada célula pasó o no los filtros

    # Es básicamente un merge de ciertas columnas de cdataDF y cfilterDF, por "ucid"

    # Columnas que tiene el filtro en "filterDF" pero que tengo que sacar de "cdataDF" antes del merge.
    drops <- c(truth_column)

    .cdataDF <- base::merge(cdataDF[, !(names(cdataDF) %in% drops)],
                            filterDF[, c(cell_unique_id_field, truth_column)],
                            by = cell_unique_id_field)
    
    print("F7.2 applyFilter")
    return(.cdataDF)
}


#' Apply shiny-filters to cdata
#' 
#' @param filters A list of polygon dataframes with columns: x (values) y (values) xvar (variable name for x values) yvar (variable name for y values) type ("Include" or "Exclude")
#' @param unique_id_fields Vector with the names of the columns which uniquely identify each observation (a "primary key") for each data point (i.e. the "ucid" is not suficcient for time series datasets).
#' @inheritParams polyFilterApply
#' @inheritParams magickCell
#' 
#' @export
#' 
apply_filters <- function(cdata, 
                          filters, 
                          unique_id_fields=c("ucid", "t.frame"),
                          truthMode = "all"){
  # Create the PK's name
  cell_unique_id_field <- paste(unique_id_fields, collapse = "_")
  
  # Check if names are in cdata
  if(!all(unique_id_fields %in% names(cdata))) 
    stop(paste0(
      "\napply_filters: error, the following id_fields were not found in cdata: '",
      paste(unique_id_fields[!unique_id_fields %in% names(cdata)], collapse = "', '"),
      "'\n"
    ))
  
  # Test if the ID column is already present
  test <- cell_unique_id_field %in% cdata
  if(test){
    # Use it in that case
    message(paste("\nThe", cell_unique_id_field, "already exists in cdata. Using it as a primary key."))
  } else {
    # Else create it from unique_id_fields
    cdata[[cell_unique_id_field]] <- apply(cdata, 1, function(d) 
      paste(d[unique_id_fields], collapse = "_")
    )
  }
  
  # Apply the filters
  capture.output({
    result <- polyFilterApply(polygon_df_list = filters, 
                              cdata = cdata,
                              truthMode = truthMode,
                              cell_unique_id_field = cell_unique_id_field
    )
  })
  
  # Save the filtered cdata
  result.cdata <- result$cdata[result$cdata$filter, ]
  
  # Remove the ID column if it was not present
  if(!test) result.cdata[[cell_unique_id_field]] <- NULL
  
  # Chin-pum!
  return(result.cdata)
}



#' Apply shiny-filters to cdata, and return \code{magick} images of each one.
#' 
#' @param strips_or_squares Produce strips (TRUE) or square-ish tiles (FALSE). Passed to magickCell's return_single_imgs.
#' @inheritParams apply_filters
#' @inheritParams polyFilterApply
#' @inheritParams magickCell
#' @inheritDotParams magickCell
#' 
#' @export
#' 
filter_group_pics <- function(cdata, 
                              paths,
                              filters, 
                              n.cells = 9,
                              unique_id_fields=c("ucid", "t.frame"),
                              truthMode = "all",
                              strips_or_squares = T,
                              ...){
  # Create the PK's name
  cell_unique_id_field <- paste(unique_id_fields, collapse = "_")
  
  # Check if names are in cdata
  if(!all(unique_id_fields %in% names(cdata))) 
    stop(paste0(
      "\napply_filters: error, the following id_fields were not found in cdata: '",
      paste(unique_id_fields[!unique_id_fields %in% names(cdata)], collapse = "', '"),
      "'\n"
    ))
  
  # Test if the ID column is already present
  test <- cell_unique_id_field %in% cdata
  if(test){
    # Use it in that case
    message(paste("\nThe", cell_unique_id_field, "already exists in cdata. Using it as a primary key."))
  } else {
    # Else create it from unique_id_fields
    cdata[[cell_unique_id_field]] <- apply(cdata, 1, function(d) 
      paste(d[unique_id_fields], collapse = "_")
    )
  }
  
  # Get the points filtered (removed) by each filter
  filters.data <- lapply(seq_along(filters), function(i){
    # Apply the filters
    capture.output({
      result <- polyFilterApply(polygon_df_list = filters[i], 
                                cdata = cdata,
                                truthMode = truthMode,
                                cell_unique_id_field = cell_unique_id_field
      )
    })
    
    # Save the filtered cdata
    result.cdata <- result$cdata[!result$cdata$filter, ]  # note the "!"
    
    # Return the data
    return(result.cdata)
  })
  # Re-set names
  names(filters.data) <- sapply(seq_along(filters), function(i){
    paste("filter", i,
          paste(filters[[i]][1, c("xvar", "yvar")], collapse = "_"),
          sep = "_")
  })
  
  # Get pictures
  pics <- lapply(filters.data, magickCell, 
                 paths=paths,
                 n.cells = n.cells, 
                 return_single_imgs = strips_or_squares, 
                 ...) %>% 
    # Append images, in case return_single_imgs=TRUE
    lapply(magick::image_append) %>% 
    # Unlist the images
    magick::image_join() %>% 
    # Add borders
    image_border_one() %>% 
    # Add filter name to the image
    magick::image_annotate(text = names(filters.data), gravity = "northwest")
  
  # Chin-pum!
  return(pics)
}

#' Plot shinyCell polygon filters
#' 
#' Useful to check out what areas the filters are covering.
#'
#' @param saved_data The output of shinyCell, or a list: \code{list(cdata = NULL, filters = saved_data$filters)}. Note that the only effect of \code{cdata = NULL} is that points will not be drawn.
#' @param print_plots Set to false to prevent printing the plots on execution.
#'
#' @importFrom rlang parse_expr
#'
#' @return A list of ggplots ready to print.
#' @export
#'
# @examples
plot_filters <- function(saved_data,
                         print_plots = TRUE){
  
  if(F){
    pgn.vars <- data.frame(xvar = c("A", "B", "C", "D"), 
                           yvar = c("D", "E", "F", "A"))
  }
  
  pgnfilters.o <- saved_data$filters %>% 
    bind_rows(.id = "polygon")
  
  pgn.vars <- pgnfilters.o |> ungroup() |> select(xvar, yvar) |> unique() #%>% bind_rows(data.frame(xvar = "el.p", yvar="a.tot"))
  
  # variables <- pgn.vars %>% select(xvar, yvar) %>% plyr::adply(.margins = 1, function(x){
  #   var_names <- c(xvar=x$xvar, yvar=x$yvar)
  #   var_names[order(var_names)]
  # }) %>% unique()
  
  variables <- pgn.vars |> ungroup() |> select(xvar, yvar) %>% 
    apply(MARGIN = 1, FUN = function(x) setNames(x[order(x)], names(x)), simplify = F) %>% 
    bind_rows() %>% unique()
    
  plot_list <- list()
  
  for(i in 1:nrow(variables))local({
    .x <- variables[i,1,drop=TRUE]
    .y <- variables[i,2,drop=TRUE]
    x_ <- rlang::parse_expr(.x)
    y_ <- rlang::parse_expr(.y)
    
    
    pgnfilters.o.unswapped <- pgnfilters.o %>% 
      filter(xvar == .x & yvar == .y) %>% 
      dplyr::rename(!!x_ := x,
                    !!y_ := y)
    pgnfilters.o.swapped <- pgnfilters.o %>% 
      filter(xvar == .y & yvar == .x) %>% 
      dplyr::rename(!!x_ := y,
                    !!y_ := x)
    
    d <- bind_rows(pgnfilters.o.unswapped, pgnfilters.o.swapped)
    
    p <- d %>%
      ggplot() +
      # geom_point(aes(x = !!x_,y = !!y_), data = filter(saved_data$cdata, t.frame == 0)) +
      {if(!is.null(saved_data$cdata)) geom_point(aes(x = !!x_,y = !!y_), data = saved_data$cdata) else NULL} +
      geom_polygon(aes(x = !!x_, 
                       y = !!y_,
                       color = polygon,
                       linetype = type),  # No se puede usar "fill", conflictua con "scale_fill" en Hex y Density
                   size = 1,
                   alpha = .1) +
      theme_minimal()
    
    if(print_plots) print(p)
    
    plot_list[[paste(.x,.y,sep="_")]] <<- p
  })
  
  return(plot_list)
}

#' Bind shinyCell polygon filters by variable pairs
#' 
#' Unites polygons with matching dimensions. Useful to check out what areas the filters are covering (see \code{plot_bound_filters}).
#'
#' @param saved_data The output of shinyCell.
#'
#' @importFrom rlang parse_expr
#'
#' @return A list of polygons bound by variable, with names unique to variable pairs (by sort). Note that "x" and "y" column names may be swapped relative to the input order.
#' @export
#'
# @examples
bind_filters <- function(saved_data,
                         print_plots = TRUE){
  
  pgnfilters.o <- saved_data$filters %>% 
    bind_rows(.id = "polygon") # %>% filter(type == .type)
  
  pgn.vars <- pgnfilters.o |> ungroup() |> select(xvar, yvar) |> unique() #%>% bind_rows(data.frame(xvar = "el.p", yvar="a.tot"))
  
  # variables <- pgn.vars %>% select(xvar, yvar) %>% plyr::adply(.margins = 1, function(x){
  #   var_names <- c(xvar=x$xvar, yvar=x$yvar)
  #   var_names[order(var_names)]
  # }) %>% unique()
  variables <- pgn.vars %>% select(xvar, yvar) %>% ungroup() |> 
    apply(MARGIN = 1, FUN = function(x) setNames(x[order(x)], names(x)), simplify = F) %>% 
    bind_rows() %>% unique()
  
  filter_list <- list()
  
  for(i in 1:nrow(variables))local({
    .x <- variables[i,1]
    .y <- variables[i,2]
    x_ <- rlang::parse_expr(.x)
    y_ <- rlang::parse_expr(.y)
    
    
    pgnfilters.o.unswapped <- pgnfilters.o %>% 
      filter(xvar == .x & yvar == .y) %>% 
      dplyr::rename(!!x_ := x,
                    !!y_ := y)
    pgnfilters.o.swapped <- pgnfilters.o %>% 
      filter(xvar == .y & yvar == .x) %>% 
      dplyr::rename(!!x_ := y,
                    !!y_ := x)
    
    d <- bind_rows(pgnfilters.o.unswapped, pgnfilters.o.swapped) %>% 
      dplyr::rename(x = !!x_,
                    y = !!y_)
    
    filter_list[[paste(.x,.y,sep="_")]] <<- d
  })
  
  return(filter_list)
}

#' Plot bound shinyCell polygon filters by variable pairs
#' 
#' Useful to check out what areas the filters are covering.
#'
#' @param bound_filters The output of \code{bind_filters}.
#'
#' @import ggplot2
#'
#' @return Plots for the polygons in bound_filters.
#' @export
#'
# @examples
plot_bound_filters <- function(bound_filters){
  plots <- 
    lapply(bound_filters, function(bfs){
      ggplot(bfs) +
        geom_polygon(aes(x=x,y=y,color=polygon,linetype=type), alpha = 0) + ggtitle(paste(bfs$xvar[1], bfs$yvar[1])) +
        theme_minimal()}
    )
  
  plots
}

# HEX FUNCTIONS ####

#' Armar data.frame para plots tipo HEXBIN con facets
#'
#' @inheritParams magickCell
#' @param facetVars Charachter vector with the names of grouping variables (tipically in rcell's \code{pdata})
#' @param varx Name of the x-axis variable.
#' @param vary Name of the y-axis variable.
#' @return A data.frame with columns: cID (bin ID), counts, x, y, and "faceting" variables.
#' @importFrom hexbin hexTapply hcell2xy
#' @importFrom data.table melt
#' @keywords internal
hexPlotDf <- function(cdata, facetVars = c("pos", "treatment"), varx = "a.tot", vary = "el.p"){
  print("F9")
  # Reemplacé las funciones anteriores por esta, que procesa un cdata y lo pone lindo para geom_hex
  # Lindo según: https://stackoverflow.com/questions/14495111/setting-hex-bins-in-ggplot2-to-same-size
  
  if(length(facetVars) != 0){
    # Hacer un dataframe con una variable "factor" que combine todos los facets,
    # de manera que identifica a cada row con el facet que le corresponde.
    
    # Paste values of the "faceting" columns to create a grouping column "id_factor"
    facet.vars.df <- cdata[,c(facetVars), drop = FALSE]
    cdata$id_factor <- as.factor(
      apply(facet.vars.df, 1, function(x) paste(x, collapse = "_"))
    )
    
    # We'll keep the uniques as metadata
    dfactor <- unique(cdata[,c("id_factor", facetVars)])
    
    # Compute the hexbins (for the entire dataset, noy by facet).
    H <- myhexbin(cdata, varx, vary, xbins = 25)
    
    # Output notes: "Returns counts for non-empty cells only".
    # H@cID:  "an integer vector of length n where cID[i] is the cell number of the i-th original point (x[i], y[i])."
    # H@cell: "vector of cell ids (cID) that can be mapped into the (x,y) bin centers in data units."
    # In other words, the IDs in the vector H@cID map the rows of the input data to hexbins identified by H@cell.
    
    # Process the output to generate hexbins coordinates and counts for each facet separately.
    # See: https://stackanswers.net/questions/setting-hex-bins-in-ggplot2-to-same-size
    # Generate a list of counts by hexbin and facet group.
    counts <- hexbin::hexTapply(hbin = H, dat = cdata$id_factor, FUN = base::table)
    # The list names are hexbin "cID"s and each one has counts for each "facet" group.
    counts <- t(base::simplify2array(counts))
    
    # Melt the counts from wide to long format (with DT, could have used tidy stuff).
    # Reemplazo reshape2 por data.table: "counts <- reshape2::melt(counts)"
    counts.dt <- data.table::as.data.table(counts)           # Convert to data.table
    counts.dt <- counts.dt[, ("cID") := H@cell]              # Add back the hexbin ID.
    counts.dt <- data.table::melt(counts.dt, id.vars="cID",  # pivot_better (?)
                                  variable.name = "id_factor",
                                  value.name = "counts")
    
    # Get XY coordinates for each hexbin, identified by its "cell" slot.
    h <- data.frame(hexbin::hcell2xy(H), cID = H@cell)
    # Then merge it by the cell slot, into the counts table.
    h <- base::merge(counts.dt, h, by = "cID")
    # Replace 0 count with NA
    h$counts[h$counts == 0] <- NA  # Para que después los valores NA no se vean (serán transparentes en ggplot)
    
    # Left-join the metadata back
    h <- base::merge(h, dfactor, by = "id_factor")
    # and remove "id_factor" column
    h$id_factor <- NULL
  } else {
    # If there are no facet groups, it is as simple as:
    H <- myhexbin(cdata, varx, vary, xbins = 25)
    h <- data.frame(hcell2xy(H), cID = H@cell, counts = H@count)
  }
  
  
  return(h)
}


#' Test plot for hexPlotDf
#' @param h The hexPlotDf output.
#' @inheritParams hexPlotDf
#' @keywords internal
hexPlotDf_test <- function(h, facetVars = c("pos", "t.frame"), varx = "a.tot", vary = "el.p"){
  # Test plot:
  axisRatio <- (max(h$x) - min(h$x))/(max(h$y) - min(h$y))
  facet <- parse(text=paste("~", paste(facetVars, collapse = " + ")))
  ggplot(h, aes(x=x, y=y, fill = counts)) +
      geom_hex(stat="identity") +
      coord_equal (axisRatio) +
      theme_bw()+ xlab(varx) + ylab(vary) +
      scale_fill_continuous (low = "grey80", high = "red", na.value = "#00000000") +
      facet_grid(facets = eval(facet))
}

#' Función hexbin
#'
#' Esencialmente \code{hexbin()} pero con el input un poco más a mi gusto.
#' 
#' La función puede fallar de forma medio oscura cuando el input tiene problemas
#' (i.e. si está vacío, o si es todo NAs, etc.)
#' 
#' @importFrom hexbin hexbin
#' @keywords internal
#' 
myhexbin <- function(bindata, varx , vary, xbins = 25){
  
  print("F10.1")
  # Ranges are single valued if drawing only one polygon, fixed here:
  bindata <- as.data.frame(bindata)
  if(nrow(bindata) == 1) {
    xbnds <- c(bindata[,varx]*0.99, bindata[,varx]*1.01)
  } else {
    xbnds <- bindata[,varx]
  }
  
  print("F10.2")
  # Ranges are single valued if drawing only one polygon, fixed here:
  if(nrow(bindata) == 1) {
    ybnds <- c(bindata[,vary]*0.99, bindata[,vary]*1.01)
  } else {
    ybnds <- bindata[,vary]
  }
  
  print("F10.3")
  h <- hexbin::hexbin(x = bindata[,varx], 
                      y = bindata[,vary], 
                      xbins = xbins,
                      IDs = TRUE,  # Add "cID" (i.e. the row index in bindata).
                      xbnds = range(xbnds),
                      ybnds = range(ybnds))
  return(h)
}
