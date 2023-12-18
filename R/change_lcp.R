#' Function to modify the fuel models in the locations targeted by the ForSys run. This function outputs
#' a new fuel model raster layer with the changed fuels, and a new landscape file (tif) reflecting the same changes.
#' Useful when re-running fire simulations (e.g. FSim) after ForSys
#'
#' @param forsys_output Treatment shapefile that resulted from a ForSysX run
#' @param landscape Landscape file used in the fire simulations that should be changed to reflect the fuel treatments in the landscape
#' @param change_rules A csv file with two columns, the first for the fuel models before and the second with the fuel models after the treatment. Must contain all the existing fuel models in the landscape file in the first column (before the treatment)
#'
#' @return
#' @export
#'
#' @examples

change_lcp <-function(forsys_output,
         landscape,
         change_rules){


  #get the name of the tif file
  landscape_all_elements <- stringr::str_split(landscape, "/", simplify=T)

  landscape_name <- landscape_all_elements[length(landscape_all_elements)]

  landscape_name <- gsub(".tif","",landscape_name)

  folder_name <- landscape_all_elements[(1:length(landscape_all_elements)-1)]

  folder_name <- paste(folder_name, collapse = '/')

  lcp <- raster::stack(landscape)

  #The fuel model map is always located in the forth position
  #plot(lcp[[4]])
  #min(getValues(lcp[[4]]))
  #max(getValues(lcp[[4]]))
  all_possible_fuel_models <- unique(raster::getValues(lcp[[4]]))


  fuel_model_map <- lcp[[4]]

  #load the rules as csv
  my_rules <- read.csv(change_rules)
  #tail(my_rules)

  #head(my_rules,15)

  #regras_use <- regras[1:12,c(6,7)]
  my_rules_use <- as.data.frame(my_rules)


  #check if all fuel models in the landscape are present in the rules
  unique_before_rules <- my_rules_use[,1][my_rules_use[,1] %in% all_possible_fuel_models]

  unique_before_rules <- unique(unique_before_rules)


  if (length(unique_before_rules) !=length(all_possible_fuel_models)){
    stop("The landscape file contains more fuel models than the ones present in the rules csv file. Please provide all the fuel models in the before treatment column.")
  }



#load the shapefile from the forsysX run
  forsys_output_shp <- sf::st_read(forsys_output)

  forsys_output_shp_treated <- subset(forsys_output_shp, Treat ==1)


  #make sure the crs are the same
  forsys_output_shp_treated <- sf::st_transform(forsys_output_shp_treated, sf::st_crs(fuel_model_map))


  r2 <- raster::crop(fuel_model_map, raster::extent(forsys_output_shp_treated))

  r3 <- raster::mask(r2, forsys_output_shp)

  r4<-raster::reclassify(r3, my_rules_use)
  #r4@data@values

  #plot(r4)




  ###delete the changed pixels from the original dataset####
  r5 <- raster::mask(fuel_model_map, forsys_output_shp,inverse=T)

  #save the new fuel model map
  x <- list(r5, r4)
  names(x) <- c("x", "y")
  x$filename <- paste(folder_name,"/","fuel_model_",landscape_name,".asc",sep="")
  x$overwrite <- TRUE
  m <- do.call(raster::merge, x)


  #save the new lcp

  lcp[[4]] <-  m
  raster::writeRaster(lcp, filename=file.path(folder_name, paste("post_forsys_",landscape_name,".tif",sep="")), options="INTERLEAVE=BAND", overwrite=TRUE)



}

