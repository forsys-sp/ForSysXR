#' An xml used in a default run
#'
#' Contains the definitions for a default ForSysXR run
#'
#' @format A xml file that defines a run
#' \describe{
#' \item {ShapeFileName}{1st input}
#' }
#' @source {Created to serve as an example}
#'
#' @examples data(xml_data)
#' data(xml_data)

"xml_data"

#' data(xml_data_threshold)

"xml_data_threshold"

#' Spatial data of stands
#'
#' `stands_data` includes real stands from a study area with three objectives that can be optimized, an area field that can be used as constraint and one threshold. It also contains fields with the stands' availability for treatments and stands to be excluded from the analysis.
#'
"stands_data"

#' Spatial data of stands with a fuelbreak network
#'
#' `stands_data_FBN` includes real stands from a study area with one objective that can be optimized, an area field that can be used as constraint and one distance to the fuelbreak network to be used as threshold. It also contains fields with the stands' availability for treatments and stands to be excluded from the analysis.
#'
"stands_data_FBN"
