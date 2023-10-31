#' Calculate spm for specified fields
#'
#' Used to normalize stands attributes by dividing each field by its maximum
#' value and multiplying by 100. Values are first divided by area if the
#' `area_field` is provides.
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
calculate_spm <- function(stands, fields=NULL, area_field=NULL, availability_txt=NULL) {

  # filter for availability
  include = TRUE
  if (!is.null(availability_txt)) {
    eval_txt <- paste0(
      "stands %>% mutate(out = ifelse(",
      availability_txt,
      ", TRUE, FALSE)) %>% pull(out)")
    include = eval(parse(text = eval_txt))
  }

  # default to calculating spm for all numeric fields if fields is null
  if (is.null(fields)) {
    x <- stands %>% lapply(is.numeric) %>% unlist()
    fields <- names(x)[x == TRUE]
  }

  for (f in fields) {
    values <- pull(stands, f)
    values[include == FALSE] <- 0
    maximum <- max(values, na.rm=T)
    spm_values <- (100 * values / maximum)
    cn <- paste0(f, "_SPM")
    stands <- stands %>% mutate(!!cn := spm_values)
  }

  return(stands)
}
