#' Calculate pcp for specified fields
#'
#' @param stands stand data
#' @param fields vector of character field names to calculate spm values
#' @param availability_txt Boolean statement describing stand availability
#'
#' @details Unavailable stands are given a value of zero.
#'
#' @importFrom dplyr pull mutate
#' @export
#'
calculate_pcp <- function(stands, fields=NULL, availability_txt=NULL){

  # filter for availability
  include = TRUE
  if (!is.null(availability_txt)) {
    eval_txt <- paste0("stands %>% mutate(out = ifelse(", availability_txt,", TRUE, FALSE)) %>% pull(out)")
    include = eval(parse(text = eval_txt))
  }

  # default to calculating spm for all numeric fields if fields is null
  if (is.null(fields)) {
    x <- stands %>% lapply(is.numeric) %>% unlist()
    fields <- names(x)[x == TRUE]
  }

  for (f in fields) {
    # calculate percent of total and multiple by 100
    cn <- paste0(f, "_PCP")
    values <- as.numeric(pull(stands, f))
    values[include == FALSE] <- 0
    sum.total <- sum(values, na.rm=T)
    stands <- stands %>% mutate(!!cn := (100 * values / sum.total))
  }

  return(stands)
}
