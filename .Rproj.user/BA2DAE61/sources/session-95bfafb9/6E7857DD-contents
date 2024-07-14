srvyr_crosstable <- function(data,
                             variables,
                             group,
                             na_values = NULL,
                             na_group = NULL,
                             fun = srvyr::survey_prop,
                             vartype = "ci",
                             envir = rlang::caller_env(),
                             ...) {

  if (is.data.frame(data)) data <- srvyr::as_survey_design(.data = data, ids = 1)

  group <- enquo(group)

  if (stringr::str_detect(expr_text(expr({{variables}})), ":|,") & !quo_is_null(group)) {

    parameter <- as.list(match.call())
    parameter <- parameter[which(names(parameter) %nin% c("variables"))[-1]]

    if (any(stringr::str_detect(sjlabelled::to_character(parameter$group), "\\{"))) {

      parameter <- purrr::map(.x = parameter,
                              ~eval(.x,
                                    envir = envir))
    }

    vars_syms <- syms(colnames(srvyr::select(data, {{variables}})))
    names(vars_syms) <- sjlabelled::get_label(srvyr::select(data, {{variables}}))

    purrr::map_dfr(.x = vars_syms,
                   function(x) {

                     parameter <- parameter %>%
                       append(., list(variables = x))

                     expr(srvyr_crosstable(!!!parameter)) %>%
                       eval_tidy(.)

                   })

  } else {

    data <- data %>%
      srvyr::select({{variables}}, {{group}})

    group_name <-  sjlabelled::get_label(data$variables[[1]])
    value_labels <- sjlabelled::get_labels(data$variables, values = "n")

    data %>%
      srvyr::mutate({{variables}} := ifelse({{variables}} %in% na_values,
                                            NA_real_,
                                            {{variables}}),
                    {{group}} := ifelse({{group}} %in% na_group,
                                        NA_real_,
                                        {{group}})) %>%
      na.omit() %>%
      srvyr::group_by({{group}}, {{variables}}) %>%
      srvyr::summarise(proportion = {{fun}}(vartype),
                       n = srvyr::survey_total()) %>%
      srvyr::ungroup() %>%
      dplyr::mutate({{group}} := sjlabelled::set_labels(x = {{group}},
                                                        labels = value_labels[[2]],
                                                        drop.na = TRUE),
                    {{variables}} := sjlabelled::set_labels(x = {{variables}},
                                                       labels = value_labels[[1]],
                                                       drop.na = TRUE),
                    n = round(n, 0),
                    group_label = sjlabelled::to_character({{group}})) %>%
      sjlabelled::drop_labels() %>%
      dplyr::rename(var_value = {{variables}},
                    group_value = {{group}}) %>%
      dplyr::mutate(var_label = group_name,
                    var_name = as_name(quo({{variables}})))

  }

}
