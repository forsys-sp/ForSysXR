#' Primary function for running the ForSysX treatment planner from R with an existent XML
#'
#' @param exe_path Path to the ForSysXConsole.exe
#' @param xml_path Path to the XML file that must be used in the ForSysX run
#'
#' @return
#' @export
#'
#' @examples
run_forsysx_xml <- function(exe_path, xml_path) {
  #run_forsysx_console(exe_path, output_xml)
  # Call a console application without showing the console window
   command <- paste(exe_path, xml_path)
   #shell(command, invisible = TRUE)
   system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)
}
