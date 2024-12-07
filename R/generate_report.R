generate_report <- function(#data,
                          output_format = rmdformats::robobook(highlight= "kate", toc_collapsed = FALSE, toc_depth = 3), #highlight: kate, toc_collapsed: FALSE, toc_depth: 3, fig_caption: yes  #toc = TRUE, toc_depth = 6, theme = "yeti"
                          output_file,
                          output_dir = getwd(),
                          y = NULL,
                          #config = configure_report(),
                          report_title = "Data Profiling Report",
                          ...) {
  ## Check if input is data.table
  # if (!is.data.table(data)) data <- data.table(data)
  # ## Check response variable
  # if (!is.null(y)) {
  #   if (!(y %in% names(data))) stop("`", y, "` not found in data!")
  # }
  ## Get directory of report markdown template
  report_dir <- system.file("rmd_template/testing_2.rmd", package = "ForSysXR")
  ## Render report into html
  suppressWarnings(rmarkdown::render(
    input = report_dir,
    output_format = output_format,
    output_file = output_file,
    output_dir = output_dir,
    intermediates_dir = output_dir,
    #params = list(data = data, report_config = config, response = y, set_title = report_title),
    ...
  ))
  ## Open report
  report_path <- path.expand(file.path(output_dir, output_file))
  browseURL(report_path)
}
