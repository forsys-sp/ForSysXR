#' Calculate PCP and SPM for specified fields
#'
#' @param stands stand data. Either loaded from the environment or path to the shapefile to be loaded.
#' @param fields vector of character field names to calculate spm values
#' @param availability_txt Boolean statement describing stand availability
#' @param output_name Path where the stand shapefile with the normalized fields should be stored. Ignored when the stands are loaded from path.
#'
#' @details Unavailable stands are given a value of zero.
#'
#' @importFrom dplyr pull mutate
#' @importFrom sf st_write
#' @export
#'
#' @examples
normalize_objectives <- function(stands, fields=NULL, availability_txt=NULL, output_name=NULL){
  #calculate_spm(stands, fields=fields, area_field=NULL, availability_txt=availability_txt)
  #calculate_pcp(stands, fields=fields, availability_txt=availability_txt)



  if(class(stands)[1]=="sf"){
    stands <- (stands)
    #input_shapefile <- paste(outputs_base_name,"_stand_data.shp",sep="")
  }

  if(class(stands)[1]=="character"){
    stands_original_name <- stands
    stands <- sf::st_read(stands)
  }



  #spm
  # filter for availability
  include = TRUE
  if (!is.null(availability_txt)) {
    eval_txt <- paste0(
      "stands %>% mutate(out = ifelse(",
      availability_txt,
      ", TRUE, FALSE)) %>% pull(out)")
    include = eval(parse(text = eval_txt))
  }

  # default to calculating spm for all numeric fields if fields is null
  if (is.null(fields)) {
    x <- stands %>% lapply(is.numeric) %>% unlist()
    fields <- names(x)[x == TRUE]
  }

  for (f in fields) {
    values <- pull(stands, f)
    values[include == FALSE] <- 0
    maximum <- max(values, na.rm=T)
    spm_values <- (100 * values / maximum)
    cn <- paste0(f, "_SPM")
    stands <- stands %>% mutate(!!cn := spm_values)
  }

  #return(stands)


  #pcp
  # filter for availability
  include = TRUE
  if (!is.null(availability_txt)) {
    eval_txt <- paste0("stands %>% mutate(out = ifelse(", availability_txt,", TRUE, FALSE)) %>% pull(out)")
    include = eval(parse(text = eval_txt))
  }

  # default to calculating spm for all numeric fields if fields is null
  if (is.null(fields)) {
    x <- stands %>% lapply(is.numeric) %>% unlist()
    fields <- names(x)[x == TRUE]
  }

  for (f in fields) {
    # calculate percent of total and multiple by 100
    cn <- paste0(f, "_PCP")
    values <- as.numeric(pull(stands, f))
    values[include == FALSE] <- 0
    sum.total <- sum(values, na.rm=T)
    stands <- stands %>% mutate(!!cn := (100 * values / sum.total))
  }


  if(exists("stands_original_name")==FALSE){
    sf::st_write(stands,paste(output_name,"forsysXR_stands_normalized.shp",sep="/"))
  } else {
    sf::st_write(stands,paste(stands_original_name),append=FALSE)
  }

  return(head(stands))

  }
