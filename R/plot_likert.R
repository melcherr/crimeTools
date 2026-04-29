#' Plot Likert Scales as Centered Stacked Bars
#'
#' Creates centered stacked bar charts for Likert-type items, supporting both even
#' and odd numbers of response categories. The function works with raw data frames
#' or `srvyr`-style survey design objects (`tbl_svy`), automatically handling
#' proportions, weights, and missing values. Multiple items and groupings can be
#' plotted together, with optional sorting, custom colors, and flexible label
#' positioning.
#'
#' @param data A data frame or survey design object of class `'tbl_svy'`.
#'   If a plain data frame is supplied, it is converted automatically with
#'   `srvyr::as_survey_design(ids = 1)`.
#' @param vars The Likert variable(s) to plot. Accepts:
#'   \itemize{
#'     \item A single variable (unquoted symbol)
#'     \item Multiple variables (e.g. `c(item1, item2, item3)` or `item1:item5`)
#'   }
#' @param group (Optional) Grouping variable for stratified plots.
#' @param grouping Character string, `"group_by_vars"` (default) or `"vars_by_group"`,
#'   controlling whether each variable is grouped by `group`, or each group gets its own set of variables.
#' @param grouping_title Logical. If `TRUE`, adds group names as plot subtitles when
#'   `grouping = "vars_by_group"`.
#' @param reverse_coding Logical. If `TRUE`, reverses the order of valid response categories.
#' @param bar_color_manual (Optional) Manual color specification as a character vector,
#'   matching the number of valid response categories.
#' @param bar_color_auto (Optional) Automatic color gradient base, given as a vector of
#'   two or more colors. Used when `bar_color_manual = NULL`.
#' @param bar_width Numeric width of bars (default `0.65`).
#' @param na_values (Optional) Vector of values in `vars` to treat as missing.
#' @param na_group (Optional) Values in `group` to treat as missing.
#' @param na_drop Logical; if `TRUE` (default), drops defined missing values. If `FALSE`,
#'   missing values are visualized according to `na_plot`.
#' @param na_plot Character; one of:
#'   \itemize{
#'     \item `'bar'` — (default) show missing values as a separate appended bar.
#'     \item `'graph'` — show missing vs. valid as a separate chart beside the main plot.
#'   }
#' @param na_bar_color,na_bar_text_color Colors for missing-value bars and their labels.
#' @param na_bar_text_nudge,na_graph_nudge Numeric offsets adjusting label positions
#'   for missing values in `'bar'` or `'graph'` mode.
#' @param na_graph_expand,na_graph_limits Additional adjustments for scaling the
#'   missing-value side chart.
#' @param na_label,valid_label Labels for missing and valid categories when
#'   `na_plot = 'graph'`.
#' @param text_position One of `"inside"`, `"outside"`, or `"outside_colored"`,
#'   controlling where percentage labels appear.
#' @param text_outside_nudge Horizontal offset for outside text.
#' @param text_color,text_middle_color Colors for text labels.
#' @param text_label_size Numeric text size for percentage labels.
#' @param show_n Logical; if `TRUE`, appends case counts (`n = ...`) to item labels.
#' @param sort Logical; if `TRUE` (default), sorts bars by response distribution.
#' @param sort_func Summary function for sorting (default: `sum`).
#' @param sort_val Vector of response category codes used for sorting.
#' @param dec Integer number of decimals in percentage labels.
#' @param axis_text_width Integer; wraps variable labels at this width.
#' @param legend Character; `"standard"` (default), `"caption"`, or `"none"`.
#' @param alpha Transparency of bars (0–1).
#' @param guide A ggplot legend guide, defaults to `guide_legend(nrow = 1, override.aes = list(alpha = 1))`.
#' @param offset_x Vector of horizontal x-axis offsets for adjusting the plot.
#' @param title,subtitle,xlab,ylab Plot title, subtitle, and axis labels.
#' @param font_family Font family for text labels.
#' @param ... Additional arguments passed to `theme_crimeTools()` for theming.
#'
#' @details
#' This function automatically handles Likert data visualization using weighted or
#' unweighted survey designs. It detects odd/even numbers of response categories and
#' centers the bars accordingly. When `na_drop = FALSE`, missing values can be visualized
#' either as a side bar (`na_plot = "bar"`) or separate graph (`na_plot = "graph"`).
#'
#' ### Key features
#' * Works with both raw and weighted (`srvyr`) data.
#' * Supports multiple Likert variables and groups.
#' * Auto-color generation or manual specification.
#' * Options for label positions, text color, and missing-value visualization.
#' * Handles both even and odd response scales symmetrically.
#'
#' @return
#' Either a single `ggplot2::ggplot` object, or a `list`/`patchwork` layout when
#' multiple groupings or `na_plot = "graph"` are used.
#'
#' @examples
#' library(sjmisc)
#' data(efc)
#'
#' # Simple Likert plot
#' plot_likert(data = efc, vars = c82cop1:c90cop9)
#'
#' # Plot Likert items by gender
#' plot_likert(data = efc, vars = c82cop1:c90cop9, group = e16sex)
#'
#' # Weighted dataset example
#' library(srvyr)
#' efc_weighted <- as_survey_design(efc, ids = 1)
#' plot_likert(data = efc_weighted, vars = c82cop1:c90cop9)
#'
#' @seealso
#' * [sjPlot::plot_likert()] for a high-level alternative.
#' * [srvyr::as_survey_design()] for creating weighted designs.
#'
#' @keywords visualization survey likert ggplot srvyr
#' @export

plot_likert <- function(data,
                        vars,
                        group = NULL,
                        grouping = "group_by_vars",
                        grouping_title = FALSE,
                        reverse_coding = FALSE,
                        bar_color_manual = NULL,
                        bar_color_auto = NULL,
                        bar_width = 0.65,
                        na_values = NULL,
                        na_group = NULL,
                        na_drop = TRUE,
                        na_plot = "bar",
                        na_bar_color = "grey30",
                        na_bar_text_color = "grey",
                        na_bar_text_nudge = 0.05,
                        na_graph_nudge = 0.1,
                        na_graph_expand = expansion(0.015,0),
                        na_graph_limits = c(-0.40, 1),
                        na_label = "missing values",
                        valid_label = "valid values",
                        text_position = "inside",
                        text_outside_nudge = 0.05,
                        text_color = "black",
                        text_middle_color = "black",
                        text_label_size = 4,
                        show_n = FALSE,
                        sort = TRUE,
                        sort_func = sum,
                        sort_val = c(1, 2),
                        dec = 0,
                        axis_text_width = 35,
                        legend = "standard",
                        alpha = 1,
                        guide = guide_legend(nrow = 1, override.aes = list(alpha = 1)),
                        offset_x = c(0, 0),
                        title = NULL,
                        subtitle = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        font_family = "",
                        ...) {

  if (!inherits(data, c("data.frame", "tbl_svy"))) {

    stop("'data' must be a data frame or an object of class 'tbl_svy'.", call. = FALSE)

  }

  vars_check <- rlang::enquo(vars)
  group_check <- rlang::enquo(group)

  if (rlang::quo_is_missing(vars_check)) {

    stop("You must supply one or more variables to 'vars'.", call. = FALSE)

  }

  if (!rlang::quo_is_null(group_check) && rlang::quo_is_missing(group_check)) {

    stop("Argument 'group' is invalid — check your syntax (unquoted variable names).", call. = FALSE)

  }

  if (!is.null(bar_color_manual) && !is.character(bar_color_manual)) {

    stop("'bar_color_manual' must be a character vector of colors.", call. = FALSE)

  }

  if (!is.null(bar_color_auto) && (!is.character(bar_color_auto) || length(bar_color_auto) < 2)) {

    stop("'bar_color_auto' must be a character vector of length >= 2.", call. = FALSE)

  }

  if (!text_position %in% c("inside", "outside", "outside_colored")) {

    stop("Invalid 'text_position'. Must be one of 'inside', 'outside', 'outside_colored'.", call. = FALSE)

  }

  if (!legend %in% c("standard", "caption", "none")) {

    stop("Invalid 'legend'. Must be one of 'standard', 'caption', or 'none'.", call. = FALSE)

  }

  if (!grouping %in% c("group_by_vars", "vars_by_group")) {

    stop("Invalid 'grouping'. Must be 'group_by_vars' or 'vars_by_group'.", call. = FALSE)

  }

  if (!is.logical(reverse_coding) || length(reverse_coding) != 1) {

    stop("'reverse_coding' must be TRUE or FALSE.", call. = FALSE)

  }


  if (is.data.frame(data)) data <- srvyr::as_survey_design(.data = data, ids = 1)


  env <- environment()

  group <- rlang::enexpr(group)

  if (!is_symbol(group) & !quo_is_null(enquo(group))) group <- parse_expr(group)

  group_enquo <- enquo(group)

  if (grouping %nin% c("group_by_vars", "vars_by_group") & !quo_is_null(group_enquo)) {

    stop("grouping must take values 'group_by_vars' or 'vars_by_group'")

  }


  if (!quo_is_null(group_enquo) & grouping == "vars_by_group") {

    parameter <- as.list(match.call())
    parameter <- parameter[which(names(parameter) %nin% c("data", "group", "grouping"))[-1]]

    data_label <- data$variables %>%
      dplyr::select({{vars}})

    value_labels <- sjlabelled::get_labels(data_label, drop.unused = TRUE, values = "n")
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

                   expr(plot_likert(!!!parameter)) %>%
                     eval_tidy(.)

                 })

  } else if (stringr::str_detect(expr_text(expr({{vars}})), ":|,") & !quo_is_null(group_enquo) & grouping == "group_by_vars") {

    warning("Multiple 'vars' arguments in conjunction with 'group' is experimental. Use 'Map()' oder 'pmap' for listwise customization.")

    parameter <- as.list(match.call())
    parameter <- parameter[which(names(parameter) %nin% c("vars"))[-1]]

    vars_syms <- syms(colnames(srvyr::select(data, {{vars}})))
    var_labels <- data$variables %>%
      dplyr::select({{vars}}) %>%
      sjlabelled::get_label()

    if (!grouping_title) var_labels <- rep(list(NULL), length(var_labels))

    purrr::map2(.x = vars_syms,
                .y = var_labels,
                function(x, y) {

                  parameter <- parameter %>%
                    append(., list(vars = x,
                                   subtitle = y))

                  expr(plot_likert(!!!parameter)) %>%
                    eval_tidy(.)

                })

  } else {

    if (na_drop) {

      if (!quo_is_null(group_enquo)) {

        srvy_data <- data %>%
          srvyr::mutate(srvyr::across(c({{vars}}, {{group}}),
                                      ~sjlabelled::as_labelled(.))) %>%
          srvyr_crosstable(data = .,
                           variables = {{vars}},
                           group = {{group}},
                           na_values = na_values,
                           na_group = na_group) %>%
          dplyr::rename(label = group_label,
                        value = var_value,
                        var = group_value) %>%
          dplyr::mutate(label = stringr::str_wrap(label, axis_text_width))

      } else {

        srvy_data <- data %>%
          srvyr::mutate(srvyr::across({{vars}},
                                      ~sjlabelled::as_labelled(.))) %>%
          srvyr_aggregation({{vars}}, na_values = na_values) %>%
          dplyr::mutate(label = stringr::str_wrap(label,
                                                  axis_text_width))

      }

      if (reverse_coding) {

        srvy_data <- srvy_data %>%
          dplyr::mutate(value = sjmisc::rec(sjlabelled::drop_labels(value), rec = "rev"))

      }

      if (show_n) {

        srvy_data <- srvy_data %>%
          dplyr::group_by(label) %>%
          dplyr::mutate(label = paste0(label, "\nn = ",
                                       round(sum(n), 0))) %>%
          dplyr::ungroup()

      }

      if (sort == TRUE) {

        srvy_complete <- srvy_data %>%
          dplyr::arrange(var) %>%
          dplyr::pull(label) %>%
          unique() %>%
          na.omit()

        srvy_data_srt <- srvy_data %>%
          tidyr::complete(data = .,
                          value,
                          var,
                          fill = list(proportion = 0, n = 0)) %>%
          dplyr::group_by(value) %>%
          dplyr::mutate(label = dplyr::coalesce(label, srvy_complete)) %>%
          dplyr::filter(value %in% sort_val) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(var, label) %>%
          dplyr::summarize(proportion = {{sort_func}}(proportion)) %>%
          dplyr::arrange(proportion) %>%
          dplyr::ungroup() %>%
          suppressMessages()

        srt_levels <- srvy_data_srt[["var"]]
        srt_labels <- srvy_data_srt[["label"]]

      } else {

        srt_levels <- unique(srvy_data[["var"]])
        srt_labels <- unique(srvy_data[["label"]])

      }

      srvy_data <- srvy_data %>%
        dplyr::mutate(prop_label = ifelse(proportion > 0,
                                          to_string_pct(proportion, dec = dec, fac = 100),
                                          NA_character_),
                      var = factor(var,
                                   levels = srt_levels,
                                   labels = srt_labels))

    } else {

      if (!quo_is_null(group_enquo)) {

        srvy_data <- data %>%
          srvyr::mutate(dplyr::across(c({{vars}}, {{group}}),
                                      ~sjlabelled::as_labelled(.))) %>%
          srvyr_crosstable(data = .,
                           variables = {{vars}},
                           group = {{group}},
                           na_group = na_group) %>%
          dplyr::rename(label = group_label,
                        value = var_value,
                        var = group_value)

      } else {

        srvy_data <- data %>%
          srvyr::mutate(srvyr::across({{vars}},
                                      ~sjlabelled::as_labelled(.))) %>%
          srvyr_aggregation({{vars}})

      }

      srvy_complete <- srvy_data %>%
        dplyr::arrange(var) %>%
        dplyr::pull(label) %>%
        unique() %>%
        na.omit()

      srvy_data <- srvy_data %>%
        tidyr::complete(data = .,
                        var,
                        value,
                        fill = list(n = 0, proportion = 0)) %>%
        dplyr::group_by(value) %>%
        dplyr::mutate(label = dplyr::coalesce(label, srvy_complete)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(prop_label = ifelse(proportion > 0,
                                          to_string_pct(proportion, dec = dec, fac = 100),
                                          NA_character_),
                      label = stringr::str_wrap(label, axis_text_width))

      if (show_n) {

        srvy_data <- srvy_data %>%
          dplyr::group_by(label) %>%
          dplyr::mutate(label = paste0(label, "\nn = ",
                                       round(sum(n), 0))) %>%
          dplyr::ungroup()

      }

      srvy_data_missing <- srvy_data %>%
        dplyr::filter(value %in% na_values)

      srvy_data <- srvy_data %>%
        dplyr::filter(value %nin% na_values)

      if (reverse_coding) {

        srvy_data <- srvy_data %>%
          dplyr::mutate(value = sjmisc::rec(sjlabelled::drop_labels(value), rec = "rev"))

      }

      if (sort == TRUE) {

        srvy_complete <- srvy_data %>%
          dplyr::arrange(var) %>%
          dplyr::pull(label) %>%
          unique() %>%
          na.omit()

        srvy_data_srt <- srvy_data %>%
          tidyr::complete(data = .,
                          value,
                          var,
                          fill = list(proportion = 0, n = 0)) %>%
          dplyr::group_by(value) %>%
          dplyr::mutate(label = dplyr::coalesce(label, srvy_complete)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(value %in% sort_val) %>%
          dplyr::group_by(var, label) %>%
          dplyr::summarize(proportion = {{sort_func}}(proportion)) %>%
          dplyr::arrange(proportion) %>%
          dplyr::ungroup() %>%
          suppressMessages()

        srt_levels <- srvy_data_srt[["var"]]
        srt_labels <- srvy_data_srt[["label"]]

      } else {

        srt_levels <- unique(srvy_data[["var"]])
        srt_labels <- unique(srvy_data[["label"]])

      }

      purrr::map(.x = list("srvy_data" = srvy_data,
                           "srvy_data_missing" = srvy_data_missing),
                 ~dplyr::mutate(.x, var = factor(var,
                                                 levels = srt_levels,
                                                 labels = srt_labels))) %>%
        list2env(x = ., envir = env)

    }

    categories <- srvy_data[["value"]] %>% unique()
    n_categories <- length(categories)
    labels <- sjlabelled::get_labels(srvy_data[["value"]], values = "n", drop.unused = TRUE)

    if (is.null(bar_color_auto)) bar_color_auto <- viridis::viridis(n_categories)

    if (!is.null(bar_color_manual)) {


      if (!na_drop & na_plot == "bar") {

        labels <- c(labels, na_label)
        bar_color <- c(bar_color_manual, na_bar_color)

      } else bar_color <- bar_color_manual


    }  else if (is.null(bar_color_manual)) {


      if (!na_drop & na_plot == "bar") {

        n_colors <- length(labels)
        bar_color <- colorRampPalette(bar_color_auto)(n_colors)

        labels <- c(labels, na_label)
        bar_color <- c(bar_color, na_bar_color)

      } else {

        n_colors <- length(labels)
        bar_color <- colorRampPalette(bar_color_auto)(n_colors)

      }

    }

    if ((n_categories %% 2) == 1) {

      middle_category <- categories[ceiling(n_categories/2)]

      srvy_data_left <- srvy_data %>%
        dplyr::filter(value <= middle_category) %>%
        dplyr::group_by(var) %>%
        dplyr::mutate(x = -1 * proportion,
                      x = ifelse(value == middle_category, x/2, x),
                      value = as.character(value))

      srvy_data_right <- srvy_data %>%
        dplyr::filter(value >= middle_category) %>%
        dplyr::group_by(var) %>%
        dplyr::mutate(x = proportion,
                      x = ifelse(value == middle_category, x/2, x),
                      prop_label = ifelse(value == middle_category,
                                          NA_character_,
                                          prop_label),
                      value = as.character(value))

      if (reverse_coding) {

        srvy_data_left <- srvy_data_left %>%
          dplyr::mutate(x_text = cumsum(x) - x/2)

        srvy_data_right <- srvy_data_right %>%
          dplyr::mutate(prop_label = prop_label,
                        x_text = rev(cumsum(rev(x)) - rev(x)/2))

      } else {

        srvy_data_right <- srvy_data_right %>%
          dplyr::mutate(x_text = cumsum(x) - x/2)

        srvy_data_left <- srvy_data_left %>%
          dplyr::mutate(prop_label = prop_label,
                        x_text = rev(cumsum(rev(x)) - rev(x)/2))

      }

      if (text_position == "inside") {

        srvy_data_text <-
          srvy_data_left %>%
          dplyr::bind_rows(srvy_data_right) %>%
          dplyr::distinct(var, value, .keep_all = TRUE) %>%
          dplyr::mutate(value = as.numeric(value),
                        x_text = ifelse(value == middle_category, 0, x_text))

      } else if (text_position %in% c("outside_colored", "outside")) {

        srvy_data_text <-
          srvy_data_left %>%
          dplyr::bind_rows(srvy_data_right) %>%
          dplyr::distinct(var, value, .keep_all = TRUE) %>%
          dplyr::mutate(value = as.numeric(value),
                        x_text = ifelse(value == middle_category, 0, x_text),
                        value = dplyr::case_when(value < middle_category ~ "low",
                                                 value == middle_category ~ "middle",
                                                 value > middle_category ~ "high"),
                        x_text = x_text + x/2) %>%
          dplyr::group_by(var, value) %>%
          dplyr::summarise(prop_label = to_string_pct(sum(proportion), dec = dec, fac = 100),
                           x_text = dplyr::case_when(value == "low" ~ min(x_text) - text_outside_nudge,
                                                     value == "middle" ~ 0,
                                                     value == "high" ~ max(x_text) + text_outside_nudge)) %>%
          suppressMessages() %>%
          dplyr::distinct()

      }

    } else {

      categories <- sort(categories)
      middle_category <- categories[n_categories/2]

      srvy_data_left <- srvy_data %>%
        dplyr::filter(value <= middle_category) %>%
        dplyr::group_by(var) %>%
        dplyr::mutate(value = sjmisc::rec(sjlabelled::drop_labels(value), rec = "rev"),
                      value = as.character(value),
                      x = -1 * rev(proportion))

      srvy_data_right <- srvy_data %>%
        dplyr::filter(value > middle_category) %>%
        dplyr::group_by(var) %>%
        dplyr::mutate(x = proportion,
                      value = as.character(value))

      if (reverse_coding) {

        srvy_data_left <- srvy_data_left %>%
          dplyr::mutate(prop_label = rev(prop_label),
                        x_text = rev(cumsum(rev(x)) - rev(x)/2))

        srvy_data_right <- srvy_data_right %>%
          dplyr::mutate(x_text = rev(cumsum(rev(x)) - rev(x)/2))

      } else {

        srvy_data_left <- srvy_data_left %>%
          dplyr::mutate(prop_label = rev(prop_label),
                        x_text =  cumsum(x) - x/2)

        srvy_data_right <- srvy_data_right %>%
          dplyr::mutate(x_text = cumsum(x) - x/2)

      }

      if (text_position == "inside") {

        srvy_data_text <-
          srvy_data_left %>%
          dplyr::bind_rows(srvy_data_right) %>%
          dplyr::distinct(var, value, .keep_all = TRUE)

      } else if (text_position %in% c("outside_colored", "outside")) {

        srvy_data_text <-
          srvy_data_left %>%
          dplyr::bind_rows(srvy_data_right) %>%
          dplyr::distinct(var, value, .keep_all = TRUE) %>%
          dplyr::mutate(value = as.numeric(value),,
                        value = dplyr::case_when(value <= middle_category ~ "low",
                                                 value > middle_category ~ "high"),
                        x_text = x_text + x/2) %>%
          dplyr::group_by(var, value) %>%
          dplyr::summarise(proportion = sum(proportion, na.rm = TRUE),
                           prop_label = ifelse(proportion > 0,
                                               to_string_pct(proportion, dec = dec, fac = 100),
                                               NA_character_),
                           x_text = dplyr::case_when(value == "low" ~ min(x_text) - text_outside_nudge,
                                                     value == "high" ~ max(x_text) + text_outside_nudge)) %>%
          suppressMessages() %>%
          dplyr::distinct()

      }

    }

    g <- ggplot() +
      geom_bar(data = srvy_data_left,
               aes(y = var,
                   x = x,
                   fill = value),
               position = position_stack(reverse = FALSE),
               stat = "identity",
               alpha = alpha,
               width = bar_width) +
      geom_bar(data = srvy_data_right,
               aes(y = var,
                   x = x,
                   fill = value),
               position = position_stack(reverse = TRUE),
               alpha = alpha,
               stat = "identity",
               width = bar_width) +
      geom_vline(xintercept = 0, color = "grey40", size = 0.75) +
      scale_fill_manual("",
                        values = bar_color,
                        labels = labels,
                        guide = guide) +
      scale_x_continuous(expand = expansion(0.025,  0),
                         limits = c(-1 - offset_x[[1]],
                                    1 + offset_x[[2]]),
                         breaks = seq(-1, 1, 0.2),
                         labels = scales::percent(abs(seq(-1, 1, 0.2)))) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      labs(title = title, subtitle = subtitle) +
      lemon::coord_capped_cart(bottom = "both") +
      theme_crimeTools(...)

    if (text_position %in% c("inside", "outside")) {

      g <- g + ggrepel::geom_text_repel(data = srvy_data_text,
                                        aes(x = x_text,
                                            y = var,
                                            label = prop_label),
                                        color = text_color,
                                        family = font_family,
                                        size = text_label_size,
                                        box.padding = unit(0, "mm"),
                                        point.size = NA,
                                        direction = "y",
                                        force = 0.5,
                                        segment.size = Inf,
                                        na.rm = TRUE)

    }

    if (text_position == "outside_colored") {

      if (na_plot == "bar" & !na_drop) n_color <- length(bar_color) - 1

      else n_color <- length(bar_color)

      text_color <- c("low" = bar_color[[1]],
                      "middle" = text_middle_color,
                      "high" = bar_color[[n_color]])

      g <- g + ggrepel::geom_text_repel(data = srvy_data_text,
                                        aes(x = x_text,
                                            y = var,
                                            label = prop_label,
                                            color = value),
                                        family = font_family,
                                        size = text_label_size,
                                        show.legend = FALSE,
                                        box.padding = unit(0, "mm"),
                                        point.size = NA,
                                        direction = "y",
                                        force = 0.5,
                                        segment.size = Inf,
                                        na.rm = TRUE) +
        scale_color_manual(values = text_color)

    }

    if (legend == "caption") {

      g <- g +
        labs(caption = glue::glue_collapse(x = glue::glue('<span style="color:{bar_color}">{labels}</span>'),
                                           sep = "   <span style='color:grey80'>|</span>   ")) +
        theme(legend.position = "none")

    } else if (legend == "none")  g <- g + theme(legend.position = "none")


    if (na_plot == "graph" & !na_drop & !is.null(na_values)) {

      g_missing <-
        srvy_data %>%
        dplyr::bind_rows(srvy_data_missing) %>%
        dplyr::mutate(missings = ifelse(value %in% na_values,
                                        na_label,
                                        valid_label)) %>%
        dplyr::group_by(var, label, missings) %>%
        dplyr::summarise(value = sum(n)) %>%
        dplyr::group_by(var) %>%
        dplyr::mutate(perc = value/sum(value),
                      perc_text = ifelse(perc > 0,
                                         to_string_pct(perc, dec = dec, fac = 100),
                                         NA_character_),
                      perc = ifelse(missings == na_label, perc * -1, perc),
                      x_text = ifelse(perc < 0,
                                      perc - na_graph_nudge,
                                      perc / 2))  %>%
        suppressMessages() %>%
        ggplot(data = ., aes(x = perc,
                             y = var,
                             fill = missings)) +
        geom_bar(stat = "identity", width = bar_width, alpha = alpha) +
        geom_vline(xintercept = 0, color = "grey40", size = 0.75) +
        geom_text(aes(x = x_text,
                      color = missings,
                      label = perc_text),
                  family = font_family,
                  size = text_label_size,
                  show.legend = FALSE,
                  na.rm = TRUE) +
        scale_color_manual("", values = c("#b03060",na_bar_text_color)) +
        scale_fill_manual("", values = c("#b03060", na_bar_color)) +
        scale_x_continuous(expand = na_graph_expand,
                           limits = na_graph_limits,
                           breaks = seq(-1, 1, 0.2),
                           labels = scales::percent(abs(seq(-1, 1, 0.2)))) +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        lemon::coord_capped_cart(bottom = "both") +
        theme_crimeTools(...)

      if (legend == "caption") {

        g_missing <- g_missing +
          labs(caption = glue::glue_collapse(x = glue::glue('<span style="color:{c("#b03060", na_bar_color)}">{c(na_label, valid_label)}</span>'),
                                             sep = "   <span style='color:grey80'>|</span>   ")) +
          theme(legend.position = "none")

      } else if (legend == "none") g_missing <- g_missing + theme(legend.position = "none")

      (g | g_missing) + patchwork::plot_layout(ncol = 2, width = c(0.7, 0.3))


    } else if (na_plot == "bar" & !na_drop & !is.null(na_values)) {

      srvy_data_missing <- srvy_data_missing %>%
        dplyr::group_by(var) %>%
        dplyr::summarise(x = sum(proportion),
                         value = na_label) %>%
        dplyr::mutate(xmin = 1 - x,
                      xmax = 1,
                      ymin = as.numeric(var) - bar_width/2,
                      ymax = as.numeric(var) + bar_width/2,
                      prop_label = ifelse(x > 0,
                                          to_string_pct(x, dec = dec, fac = 100),
                                          NA_character_)) %>%
        suppressMessages()

      if (nrow(srvy_data_missing) > 0) {

        g +
          ggplot2::geom_rect(data = srvy_data_missing,
                             aes(ymin = ymin,
                                 ymax = ymax,
                                 xmin = xmin,
                                 xmax = xmax,
                                 fill = value),
                             show.legend = TRUE,
                             alpha = alpha) +
          ggrepel::geom_text_repel(data = srvy_data_missing,
                                   aes(x = xmin - na_bar_text_nudge,
                                       y = var,
                                       label = prop_label),
                                   family = font_family,
                                   color = na_bar_text_color,
                                   size = text_label_size,
                                   box.padding = unit(0, "mm"),
                                   point.size = NA,
                                   direction = "y",
                                   force = 0.5,
                                   segment.size = Inf,
                                   na.rm = TRUE)

      } else g

    } else g

  }

}
