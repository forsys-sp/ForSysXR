configure_report <- function(
    add_introduce = TRUE,
    add_plot_intro = TRUE,
    add_plot_str = TRUE,
    add_plot_missing = TRUE,
    add_plot_histogram = TRUE,
    add_plot_density = FALSE,
    add_plot_qq = TRUE,
    add_plot_bar = TRUE,
    add_plot_correlation = TRUE,
    add_plot_prcomp = TRUE,
    add_plot_boxplot = TRUE,
    add_plot_scatterplot = TRUE,
    introduce_args = list(),
    plot_intro_args = list(),
    plot_str_args = list(type = "diagonal", fontSize = 35, width = 1000, margin = list(left = 350, right = 250)),
    plot_missing_args = list(),
    plot_histogram_args = list(),
    plot_density_args = list(),
    plot_qq_args = list(sampled_rows = 1000L),
    plot_bar_args = list(),
    plot_correlation_args = list("cor_args" = list("use" = "pairwise.complete.obs")),
    plot_prcomp_args = list(),
    plot_boxplot_args = list(),
    plot_scatterplot_args = list(sampled_rows = 1000L),
    global_ggtheme = quote(theme_gray()),
    global_theme_config = list()
) {
  ## Parse formal arguments
  input_args <- as.list(match.call())
  self_name <- input_args[[1]]
  formal_args <- formals(match.fun(self_name))
  switches <- grep("add_", names(formal_args), value = TRUE, fixed = TRUE)
  global_settings <- grep("global_", names(formal_args), value = TRUE, fixed = TRUE)
  global_exceptions <- c("add_introduce", "add_plot_str")
  ## Set config data based on arguments
  config <- lapply(setNames(switches, switches), function(s) {
    if ((!is.null(input_args[[s]]) && eval(input_args[[s]])) || (is.null(input_args[[s]]) && formal_args[[s]])) {
      key_args <- paste0(gsub("add_", "", s, fixed = TRUE), "_args")
      input_values <- eval(input_args[[key_args]])
      formal_values <- eval(formal_args[[key_args]])
      value <- NULL
      if (!(s %in% global_exceptions)) {
        if ("ggtheme" %in% names(input_values)) {
          value <- list(ggtheme = input_values[["ggtheme"]])
          input_values[["ggtheme"]] <- NULL
        } else {
          value <- list(ggtheme = global_ggtheme)
        }
        if ("theme_config" %in% names(input_values)) {
          value <- c(value, list(theme_config = input_values[["theme_config"]]))
          input_values[["theme_config"]] <- NULL
        } else {
          value <- c(value, list(theme_config = global_theme_config))
        }
      }
      if (!is.null(input_values)) {
        value <- c(value, input_values)
      } else {
        value <- c(value, formal_values)
      }
    }
  })
  names(config) <- gsub("add_", "", names(config), fixed = TRUE)
  Filter(Negate(is.null), config)
}
