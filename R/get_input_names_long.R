#' A helper function to write the html report
#'
#' @return
#' @export
#'
#' @examples
get_input_names_long <- function() {
  lv <- .Last.value

  fname <- tempfile()
  savehistory(fname)
  lastcmd <- head(tail(readLines(fname), 50), 50)

  parts <- strsplit(gsub("[^[:alnum:] ]", "", lastcmd), " +")[[1]]
  vars <- as.list.environment(.GlobalEnv)
  c(list(Last.command=lastcmd, Last.value=lv), vars[names(vars) %in% parts])
}

