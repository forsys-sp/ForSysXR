#' Create a landscape file to be used in flammap, MTT or farsite fire simulations
#'
#' @param elevation Elevation (in meters) raster file
#' @param slope Slope (in degrees) raster file
#' @param aspect Aspect (in degrees) raster file
#' @param fuel_model Fuel model map (class) raster file
#' @param canopy_cover Canopy cover (as percentage) raster file
#' @param stand_height Stand height (in meters) raster file. Optional
#' @param canpy_base_height Canopy base height (in meters) raster file. Optional
#' @param canopy_bulk_density Canopy bulk density (in kg/m3) raster file. Optional
#' @param lcp_name Name of the lcp to be created
#' @param overwrite Logical. If TRUE, an lcp with the same name in the directory folder will be replaced. Default is FALSE
#'
#' @return
#' @export
#'
#' @examples
create_lcp <- function(elevation,
                       slope,
                       aspect,
                       fuel_model,
                       canopy_cover,
                       stand_height,
                       canpy_base_height,
                       canopy_bulk_density,
                       lcp_name,
                       overwrite=FALSE){


  #elevation
  if(class(elevation)[1]=="character"){
    input_elevation_format <- substrRight(elevation,4)
    if(input_elevation_format!= ".asc" & input_elevation_format!= ".tif")
      stop("elevation has to be a raster file!")}


  if(class(elevation)[1]=="RasterLayer"){
    elevation_raster <- elevation
  }else{
    elevation_raster <- raster::raster(elevation)
  }

  #slope
  if(class(slope)[1]=="character"){
    input_slope_format <- substrRight(slope,4)
    if(input_slope_format!= ".asc" & input_slope_format!= ".tif")
      stop("slope has to be a raster file!")}

  if(class(slope)[1]=="RasterLayer"){
    slope_raster <- slope
  }else{
    slope_raster <- raster::raster(slope)
  }


  #aspect

  if(class(aspect)[1]=="character"){
    input_aspect_format <- substrRight(aspect,4)
    if(input_aspect_format!= ".asc" & input_aspect_format!= ".tif")
      stop("aspect has to be a raster file!")}

  if(class(aspect)[1]=="RasterLayer"){
    aspect_raster <- aspect
  }else{
    aspect_raster <- raster::raster(aspect)
  }


  #fuel model
  if(class(fuel_model)[1]=="character"){
    input_fuel_model_format <- substrRight(fuel_model,4)
    if(input_fuel_model_format!= ".asc" & input_fuel_model_format!= ".tif")
      stop("fuel_model has to be a raster file!")}

  if(class(fuel_model)[1]=="RasterLayer"){
    fuel_model_raster <- fuel_model
  }else{
    fuel_model_raster <- raster::raster(fuel_model)
  }


  #canopy cover

  if(class(canopy_cover)[1]=="character"){
    input_canopy_cover_format <- substrRight(canopy_cover,4)
    if(input_canopy_cover_format!= ".asc" & input_canopy_cover_format!= ".tif")
      stop("canopy_cover has to be a raster file!")}

  if(class(canopy_cover)[1]=="RasterLayer"){
    canopy_cover_raster <- canopy_cover
  }else{
    canopy_cover_raster <- raster::raster(canopy_cover)
  }


  #stand height
  if(!missing(stand_height)){
    if(class(stand_height)[1]=="character"){
      input_stand_height_cover_format <- substrRight(stand_height,4)
      if(input_stand_height_cover_format!= ".asc" & input_stand_height_cover_format!= ".tif")
        stop("stand_height has to be a raster file!")}

    if(class(stand_height)[1]=="RasterLayer"){
      stand_height_raster <- stand_height
    }else{
      stand_height_raster <- raster::raster(stand_height)
    }

  }


  #canopy base height
  if(!missing(canpy_base_height)){
    if(class(canpy_base_height)[1]=="character"){
      input_canpy_base_height_format <- substrRight(canpy_base_height,4)
      if(input_canpy_base_height_format!= ".asc" & input_canpy_base_height_format!= ".tif")
        stop("canpy_base_height has to be a raster file!")}

    if(class(canpy_base_height)[1]=="RasterLayer"){
      canpy_base_height_raster <- canpy_base_height
    }else{
      canpy_base_height_raster <- raster::raster(canpy_base_height)
    }
  }


  #canopy bulk density
  if(!missing(canopy_bulk_density)){
    if(class(canopy_bulk_density)[1]=="character"){
      input_canopy_bulk_density_format <- substrRight(canopy_bulk_density,4)
      if(input_canopy_bulk_density_format!= ".asc" & input_canopy_bulk_density_format!= ".tif")
        stop("canopy_bulk_density has to be a raster file!")}

    if(class(canopy_bulk_density)[1]=="RasterLayer"){
      canopy_bulk_density_raster <- canopy_bulk_density
    }else{
      canopy_bulk_density_raster <- raster::raster(canopy_bulk_density)
    }
  }




  if(missing(stand_height)){
    stand_height_raster<-fuel_model_raster
    stand_height_raster<- raster::reclassify(stand_height_raster,c(-Inf,Inf,0))
  }

  if(missing(canpy_base_height)){
    canpy_base_height_raster<-fuel_model_raster
    canpy_base_height_raster<- raster::reclassify(canpy_base_height_raster,c(-Inf,Inf,0))
  }

  if(missing(canopy_bulk_density)){
    canopy_bulk_density_raster<-fuel_model_raster
    canopy_bulk_density_raster<- raster::reclassify(canopy_bulk_density_raster,c(-Inf,Inf,0))
  }




  trying_lcp <- raster::stack(c(elevation_raster,slope_raster,
                                aspect_raster,fuel_model_raster,canopy_cover_raster,
                                stand_height_raster,canpy_base_height_raster,canopy_bulk_density_raster))

  #maybe this part will only be when missing canopy data - Wait for Ana Barros
  names(trying_lcp)<-c("Elevation","Slope","Aspect","Fuel.Model","Canopy.Cover",
                       paste(lcp_name,"_6",sep=""),paste(lcp_name,"_7",sep=""),paste(lcp_name,"_8",sep=""))


  if(overwrite==TRUE){
    raster::writeRaster(trying_lcp, filename=paste(lcp_name,".tif",sep=""), options="INTERLEAVE=BAND",overwrite=TRUE)
  }else{
    raster::writeRaster(trying_lcp, filename=paste(lcp_name,".tif",sep=""), options="INTERLEAVE=BAND")
  }


}
