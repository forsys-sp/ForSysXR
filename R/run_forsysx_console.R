#' Title
#'
#' @param exe_path
#' @param xml_path
#'
#' @return
#' @export
#'
#' @examples

run_forsysx_console <- function(exe_path, xml_path) {

  # Call a console application without showing the console window
  command <- paste(exe_path, xml_path)
  #shell(command, invisible = TRUE)
  system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)
}
