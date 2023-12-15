#' Function to modify the fuel models in the locations targeted by the ForSys run. This function outputs
#' a new fuel model raster layer with the changed fuels, and a new landscape file (tif) reflecting the same changes.
#' Useful when re-running fire simulations (e.g. FSim) after ForSys
#'
#' @param forsys_output Treatment shapefile that resulted from a ForSysX run
#' @param landscape Landscape file used in the fire simulations that should be changed to reflect the fuel treatments in the landscape
#' @param change_rules A csv file containing the fuel models before and after treatment. Must contain all the existing fuel models in the landscape file.
#'
#' @return
#' @export
#'
#' @examples

function(forsys_output,
         landscape,
         change_rules){




}
