get_input_names <- function() {
  lv <- .Last.value

  fname <- tempfile()
  savehistory(fname)
  lastcmd <- head(tail(readLines(fname), 10), 10)

  parts <- strsplit(gsub("[^[:alnum:] ]", "", lastcmd), " +")[[1]]
  vars <- as.list.environment(.GlobalEnv)
  c(list(Last.command=lastcmd, Last.value=lv), vars[names(vars) %in% parts])
}
