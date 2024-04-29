#' Title
#'
#' @param exe_path
#' @param input_shapefile
#' @param stand_id
#' @param output_adjacency_matrix
#'
#' @return
#' @export
#'
#' @examples

gen_adj_forsysx <- function(exe_path, input_shapefile,stand_id,output_adjacency_matrix) {
  output_adjacency_matrix_use <- paste0(output_adjacency_matrix,"/adjacency_matrix_forsys.csv")

  # Call a console application without showing the console window
  command <- paste(exe_path, input_shapefile,stand_id,output_adjacency_matrix_use)
  #shell(command, invisible = TRUE)
  system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)
}
