srvyr_aggregation <- function(data,
                              vars,
                              grouped = TRUE,
                              na_values = NULL,
                              fun = srvyr::survey_mean,
                              vartype = "ci",
                              conf_level = 0.95,
                              percentile = 1,
                              ...) {

  if (is.data.frame(data)) data <- srvyr::as_survey_design(.data = data, ids = 1)

  data <- data %>%
    srvyr::select({{vars}})

  labels <- sjlabelled::get_label(data$variables)
  value_labels <- sjlabelled::get_labels(data$variables[, 1], values = "n")


  data <- data %>%
    srvyr::mutate(srvyr::across(dplyr::everything(), ~ifelse(. %in% na_values, NA_real_, .)))

  col_names <- names(data$variables)

  data <-
    purrr::map2_dfr(.x = col_names,
                    .y = labels,
                    function(x, y) {

                      x_sym <- sym(x)

                      if (grouped) {

                        data <- data %>%
                          srvyr::select({{x_sym}}) %>%
                          na.omit() %>%
                          srvyr::group_by({{x_sym}}) %>%
                          srvyr::summarise(proportion = {{fun}}(vartype = vartype,
                                                                level = conf_level)) %>%
                          dplyr::left_join(srvyr::survey_count(data, {{x_sym}})) %>%
                          srvyr::rename(value = 1) %>%
                          suppressMessages()

                      } else {

                        name <- enexpr(fun)

                        data <- data %>%
                          srvyr::mutate(srvyr::across(everything(),
                                               ~ifelse(. > quantile(., percentile, na.rm = TRUE), NA_real_, .))) %>%
                          srvyr::summarise({{name}} := {{fun}}({{x_sym}},
                                                               vartype = vartype,
                                                               level = conf_level,
                                                               na.rm = TRUE))

                      }

                      data %>% srvyr::mutate(var = x, label = y)

                    })

  if (grouped) data %>%
    srvyr::mutate(value = sjlabelled::set_labels(value,
                                                 force.values = FALSE,
                                                 labels = value_labels)) %>%
    suppressMessages()

  else data

}
