plot_prevalence <- function(data,
                            vars,
                            group = NULL,
                            prop_values,
                            graph_type = "bar",
                            na_values = NULL,
                            na_group = NULL,
                            show_n = TRUE,
                            show_confint = TRUE,
                            conf_level = 0.95,
                            sort = "asc",
                            dec = 0,
                            axis_text_width = 35,
                            ...) {

  if (is.data.frame(data)) {

    data <- srvyr::as_survey_design(.data = data, weights = {{weight}})

  }

  if (sort %nin% c("asc", "desc", "none")) {

    stop("'sort' must take values 'asc', 'desc' or 'none'")

  }

  if (length(prop_values) > 1) {

    vars_labels <-
      data$variables %>%
      select({{vars}}) %>%
      get_label(.) %>%
      syms()

    data$variables <- data$variables %>%
      mutate(across({{vars}},
                    ~case_when(. %in% prop_values ~ 1,
                               . %in% na_values ~ .,
                               TRUE ~ 0))) %>%
      var_labels(!!!vars_labels)

    prop_values <- 1

  }

  group <- enexpr(group)

  if(!is_symbol(group) & !quo_is_null(enquo(group))) group <- parse_expr(group)

  group_enquo <- enquo(group)

  if (!quo_is_null(group_enquo)) {

    srvy_data <- data %>%
      mutate(across(c({{vars}}, {{group}}), ~as_labelled(.))) %>%
      srvyr_crosstable(data = .,
                       vars = {{vars}},
                       group = {{group}},
                       na_values = na_values,
                       na_group = na_group) %>%
      mutate(across(c(group_label,var_label),
                    ~str_wrap(., axis_text_width))) %>%
      filter(var_value %in% prop_values)

    if (sort == "asc") {

      srvy_data_srt <- srvy_data %>%
        group_by(group_value) %>%
        arrange(proportion)

    } else if (sort == "desc") {

      srvy_data_srt <- srvy_data %>%
        group_by(group_value) %>%
        arrange(desc(proportion))

    } else {

      srvy_data_srt <- srvy_data

    }

    srt_levels_group <- unique(srvy_data_srt[["group_value"]])
    srt_labels_group <- unique(srvy_data_srt[["group_label"]])

    srt_levels <- unique(srvy_data_srt[["var_name"]])
    srt_labels <- unique(srvy_data_srt[["var_label"]])

    srvy_data <- srvy_data %>%
      mutate(prop_label = if_else(proportion > 0,
                                  prozent(proportion, dec = dec, fac = 100),
                                  NA_character_),
             group_value = factor(group_value,
                                  levels = srt_levels_group,
                                  labels = srt_labels_group),
             var_name = factor(var_name,
                               levels = srt_levels,
                               labels = srt_labels)) %>%
      rename(var = var_name)

    if (show_n) {

      srvy_data <- srvy_data %>%
        group_by(var, group_label) %>%
        mutate(prop_label = paste0(prop_label, " (", round(sum(n), 0), ")")) %>%
        ungroup()

    }

    group_value <- expr(group_value)

  }

  else {

    srvy_data <- data %>%
      mutate(across({{vars}},
                    ~as_labelled(.))) %>%
      srvyr_aggregation({{vars}},
                        conf_level = conf_level,
                        na_values = na_values) %>%
      mutate(label = str_wrap(label, axis_text_width)) %>%
      filter(value %in% prop_values)

    if (show_n) {

      srvy_data <- srvy_data %>%
        group_by(label) %>%
        mutate(label = paste0(label, "\nn = ", round(sum(n), 0))) %>%
        ungroup()

    }

    if (sort == "asc") {

      srvy_data_srt <- srvy_data %>%
        arrange(proportion)

    } else if (sort == "desc") {

      srvy_data_srt <- srvy_data %>%
        arrange(desc(proportion))

    } else {

      srvy_data_srt <- srvy_data

    }

    srt_levels <- srvy_data_srt[["var"]]
    srt_labels <- srvy_data_srt[["label"]]

    srvy_data <- srvy_data %>%
      mutate(prop_label = if_else(proportion > 0,
                                  prozent(proportion, dec = dec, fac = 100),
                                  NA_character_),
             var = factor(var,
                          levels = srt_levels,
                          labels = srt_labels))

    group_value <- expr(NULL)

  }

  if (graph_type == "bar") {

    srvy_data <- srvy_data %>%
      mutate(proportion_low = if_else(proportion_low < 0, 0, proportion_low))

    sips_bar(data = srvy_data,
             y = var,
             x = proportion,
             xmin = proportion_low,
             xmax = proportion_upp,
             text = prop_label,
             group = {{group_value}},
             show_confint = show_confint,
             ...)

  } else if (graph_type == "lollipop") {

    sips_lollipop(data = srvy_data,
                  y = var,
                  x = proportion,
                  text = prop_label,
                  group = {{group_value}},
                  ...)

  }

}
