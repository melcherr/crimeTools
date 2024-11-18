#' Calculates and Plots Prevalence Values
#'
#' plot_prevalence() is a wrapper around plot_bar() and plot_lollipop(). The function calculates prevalence values and their respective confidence intervals before plotting.
#' @param data Data frame
#' @param x variable names, must be a symbol or character vector
#' @param y variable names, must be a symbol or character vector
#' @param group group (optional)
#' @return A-ggplot()-object
#' @examples
#' plot_prevalence(data = iris, vars = setosa) ;
#' @export

plot_prevalence <- function(data,
                            vars,
                            group = NULL,
                            grouping = "group_by_vars",
                            grouping_title = FALSE,
                            prop_values,
                            graph_type = "bar",
                            na_values = NULL,
                            na_group = NULL,
                            show_n = TRUE,
                            show_confint = TRUE,
                            conf_level = 0.95,
                            sort = "asc",
                            group_sort_val = NULL,
                            dec = 0,
                            axis_text_width = 35,
                            ...) {

  if (is.data.frame(data)) {

    data <- srvyr::as_survey_design(.data = data, ids = 1)

  }

  if (sort %nin% c("asc", "desc", "none")) {

    stop("'sort' must take values 'asc', 'desc' or 'none'")

  }

  env <- environment()

  group <- enexpr(group)

  if(!is_symbol(group) & !quo_is_null(enquo(group))) group <- parse_expr(group)

  group_enquo <- enquo(group)


  if (!quo_is_null(group_enquo) & grouping == "vars_by_group") {

    parameter <- as.list(match.call())
    parameter <- parameter[which(names(parameter) %nin% c("data", "group", "grouping"))[-1]]

    data_label <- data$variables %>%
      dplyr::select({{vars}})

    value_labels <- sjlabelled::get_labels(data_label,
                                           drop.unused = TRUE,
                                           values = "n")
    var_labels <- sjlabelled::get_label(data_label)
    group_labels <- data$variables %>%
      srvyr::filter({{group}} %nin% na_group) %>%
      dplyr::pull({{group}}) %>%
      sjlabelled::get_labels(values = "n",
                             drop.unused = TRUE)

    if (!grouping_title) group_labels <- rep(list(NULL), length(group_labels))

    data %>%
      srvyr::filter({{group}} %nin% na_group) %>%
      dplyr::group_split({{group}}) %>%
      purrr::map2(.x = .,
                  .y = group_labels,
                  function(x, y) {

                    x$variables <- x$variables %>%
                      dplyr::select({{vars}}) %>%
                      sjlabelled::set_labels(x = ., labels = value_labels) %>%
                      sjlabelled::set_label(x = ., label = var_labels)

                    parameter <- parameter %>%
                      append(., list(data = x,
                                     subtitle = y))

                    expr(plot_prevalence(!!!parameter)) %>%
                      eval_tidy(.)

                  })


  } else {

    if (length(prop_values) > 1) {

      vars_labels <-
        data$variables %>%
        dplyr::select({{vars}}) %>%
        sjlabelled::get_label(.) %>%
        syms()

      data$variables <- data$variables %>%
        dplyr::mutate(dplyr::across({{vars}},
                                    ~dplyr::case_when(. %in% prop_values ~ 1,
                                                      . %in% na_values ~ .,
                                                      TRUE ~ 0))) %>%
        sjlabelled::var_labels(!!!vars_labels)

      prop_values <- 1

    }

    if (!quo_is_null(group_enquo)) {

      srvy_data <- data %>%
        srvyr::mutate(srvyr::across(c({{vars}}, {{group}}),
                                    ~ sjlabelled::as_labelled(.))) %>%
        srvyr_crosstable(data = .,
                         variables = {{vars}},
                         group = {{group}},
                         na_values = na_values,
                         na_group = na_group) %>%
        dplyr::mutate(dplyr::across(c(group_label, var_label),
                                    ~stringr::str_wrap(., axis_text_width))) %>%
        dplyr::filter(var_value %in% prop_values) %>%
        tidyr::complete(data = .,
                        group_value,
                        var_name,
                        fill = list(proportion = 0,
                                    proportion_upp = 0,
                                    n = 0)) %>%
        dplyr::group_by(group_value) %>%
        dplyr::arrange(group_label) %>%
        dplyr::mutate(group_label = zoo::na.locf(group_label)) %>%
        dplyr::group_by(var_name) %>%
        dplyr::arrange(var_label) %>%
        dplyr::mutate(var_label = zoo::na.locf(var_label)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(group_value)

      srvy_data

      srt_levels_group <- unique(srvy_data[["group_value"]])
      srt_labels_group <- unique(srvy_data[["group_label"]])

      min_group <- min(srvy_data[["group_value"]], na.rm = TRUE)
      max_group <- max(srvy_data[["group_value"]], na.rm = TRUE)

      if (is.null(group_sort_val)) group_sort_val <- max_group

      if (!is.null(group_sort_val) & !dplyr::between(group_sort_val, left = min_group, right = max_group)) {

        stop(paste("group_sort_val must be an integer between", min_group, "and", max_group))


      }

      if (sort == "desc") {

        srvy_data_srt <- srvy_data %>%
          dplyr::filter(group_value == group_sort_val) %>%
          dplyr::arrange(proportion)

      } else if (sort == "asc") {

        srvy_data_srt <- srvy_data %>%
          dplyr::filter(group_value == group_sort_val) %>%
          dplyr::arrange(desc(proportion))

      } else {

        srvy_data_srt <- srvy_data

      }

      srt_levels <- srvy_data_srt[["var_name"]]
      srt_labels <- srvy_data_srt[["var_label"]]

      srvy_data <- srvy_data %>%
        dplyr::mutate(prop_label = ifelse(proportion >= 0,
                                          to_string_pct(proportion, dec = dec, fac = 100),
                                          NA_character_),
                      group_value = factor(group_value,
                                           levels = srt_levels_group,
                                           labels = srt_labels_group),
                      var_name = factor(var_name,
                                        levels = srt_levels,
                                        labels = srt_labels)) %>%
        dplyr::rename(var = var_name)

      if (show_n) {

        srvy_data <- srvy_data %>%
          dplyr::group_by(var, group_value) %>%
          dplyr::mutate(prop_label = paste0(prop_label, " (", round(sum(n), 0), ")")) %>%
          dplyr::ungroup()

      }

      group_value <- expr(group_value)

    }

    else {

      varnames <- data$variables %>%
        dplyr::select({{vars}}) %>%
        sjlabelled::get_label()

      srvy_data <- data %>%
        srvyr::mutate(srvyr::across({{vars}},
                                    ~sjlabelled::as_labelled(.))) %>%
        srvyr_aggregation({{vars}},
                          conf_level = conf_level,
                          na_values = na_values) %>%
        dplyr::mutate(label = stringr::str_wrap(label, axis_text_width)) %>%
        dplyr::filter(value %in% prop_values)

      if (length(varnames) > nrow(srvy_data)) {

        var_na <- varnames[names(varnames) %nin% srvy_data[["var"]]]

        data_na <- dplyr::tibble(var = names(var_na),
                                 label = stringr::str_wrap(var_na, axis_text_width),
                                 proportion = 0,
                                 proportion_upp = 0,
                                 n = 0)

        srvy_data <- srvy_data %>%
          dplyr::bind_rows(data_na)

      }

      srvy_data

      if (show_n) {

        srvy_data <- srvy_data %>%
          dplyr::group_by(label) %>%
          dplyr::mutate(label = paste0(label, "\nn = ", round(sum(n), 0))) %>%
          dplyr::ungroup()

      }

      if (sort == "desc") {

        srvy_data_srt <- srvy_data %>%
          dplyr::arrange(proportion)

      } else if (sort == "asc") {

        srvy_data_srt <- srvy_data %>%
          dplyr::arrange(desc(proportion))

      } else {

        srvy_data_srt <- srvy_data

      }

      srt_levels <- srvy_data_srt[["var"]]
      srt_labels <- srvy_data_srt[["label"]]

      srvy_data <- srvy_data %>%
        dplyr::mutate(prop_label = ifelse(proportion >= 0,
                                          to_string_pct(proportion, dec = dec, fac = 100),
                                          NA_character_),
                      var = factor(var,
                                   levels = srt_levels,
                                   labels = srt_labels))

      group_value <- expr(NULL)

    }

    if (graph_type == "bar") {

      srvy_data <- srvy_data %>%
        dplyr::mutate(proportion_low = ifelse(proportion_low < 0,
                                              0,
                                              proportion_low))

      plot_bar(data = srvy_data,
               y = var,
               x = proportion,
               xmin = proportion_low,
               xmax = proportion_upp,
               text = prop_label,
               group = {{group_value}},
               show_confint = show_confint,
               ...)

    } else if (graph_type == "lollipop") {

      plot_lollipop(data = srvy_data,
                    y = var,
                    x = proportion,
                    text = prop_label,
                    group = {{group_value}},
                    ...)

    }

  }

}
