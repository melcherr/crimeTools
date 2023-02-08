#' Plot Likert Scales as Centered Stacked Bars
#'
#' Plots likert scales with even and odd number of categories as centered stacked bar charts with and without missing values.
#' @param data Data frame
#' @return A-ggplot2::ggplot()-object or a list()
#' @examples
#' plot_likert(data = iris, vars = setosa) ;
#' @export

plot_likert <- function(data,
                        vars,
                        group = NULL,
                        weight = NULL,
                        reverse_coding = FALSE,
                        bar_color_manual = NULL,
                        bar_color_auto = c("#3e6487", "#e36c33"),
                        bar_width = 0.65,
                        na_values = NULL,
                        na_group = NULL,
                        na_label = "fehlende Werte",
                        na_drop = TRUE,
                        na_plot = "bar",
                        na_bar_color = "grey30",
                        na_bar_text_color = "grey",
                        na_bar_text_nudge = 0.05,
                        na_graph_nudge = 0.1,
                        na_graph_expand = c(0.015,0),
                        na_graph_limits = c(-0.40, 1),
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
                        legend_size = 12,
                        legend_just = "center",
                        legend_position = "bottom",
                        alpha = 1,
                        guide = guide_legend(nrow = 1, override.aes = list(alpha = 1)),
                        title = NULL,
                        subtitle = NULL,
                        axis_text_y_size = 11,
                        title_position = "plot",
                        font_family = "Calibri",
                        subtitle_size = 16) {

  if (text_position %nin% c("inside", "outside", "outside_colored")) {

    stop("text_position must take values 'inside', 'outside' or 'outside_colored'")

  }

  if (legend %nin% c("standard", "caption", "none")) {

    stop("legend must take values 'standard', 'caption' or 'none'")

  }


  env <- environment()

  group <- rlang::enexpr(group)

  if(!is_symbol(group) & !quo_is_null(enquo(group))) group <- parse_expr(group)

  group_enquo <- enquo(group)

  if (stringr::str_detect(expr_text(expr({{vars}})), ":|,") & !quo_is_null(group_enquo)) {

    warning("Multiple 'vars' arguments in conjunction with 'group' is experimental. Use 'Map()' oder 'pmap' for listwise customization.")

    parameter <- as.list(match.call())
    parameter <- parameter[which(names(parameter) %nin% c("vars"))[-1]]

    vars_syms <- syms(colnames(srvyr::select(data, {{vars}})))

    purrr::map(.x = vars_syms,
               function(x) {

                 parameter <- parameter %>%
                   append(., list(vars = x))

                 expr(plot_likert(!!!parameter)) %>%
                   eval_tidy(.)

               })

  } else {

    if (is.data.frame(data)) data <- srvyr::as_survey_design(.data = data, weights = {{weight}}, ids = 1)

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
        tidyr::complete(data = ., var,
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
    labels <- sjlabelled::get_labels(sjlabelled::drop_labels(srvy_data[["value"]]), values = "n")

    if (!is.null(bar_color_manual)) {


      if (!na_drop & na_plot == "bar") {

        bar_color <- c(bar_color_manual, na_bar_color)
        labels <- c(labels, na_label)

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
          dplyr::mutate(x_text = ifelse(value == middle_category, 0, x_text))

      } else if (text_position %in% c("outside_colored", "outside")) {

        srvy_data_text <-
          srvy_data_left %>%
          dplyr::bind_rows(srvy_data_right) %>%
          dplyr::distinct(var, value, .keep_all = TRUE) %>%
          dplyr::mutate(x_text = ifelse(value == middle_category, 0, x_text),
                        value = dplyr::case_when(value < middle_category ~ "low",
                                                 value == middle_category ~ "middle",
                                                 value > middle_category ~ "high"),
                        x_text = x_text + x/2) %>%
          dplyr::group_by(var, value) %>%
          dplyr::summarise(prop_label = to_string_pct(sum(proportion), dec = dec, fac = 100),
                           x_text = case_when(value == "low" ~ min(x_text) - text_outside_nudge,
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
          dplyr::mutate(value = dplyr::case_when(value <= middle_category ~ "low",
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
      scale_x_continuous(expand = c(0.025,0),
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, 0.2),
                         labels = scales::percent(abs(seq(-1, 1, 0.2)))) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      labs(title = title, subtitle = subtitle) +
      lemon::coord_capped_cart(bottom = "both") +
      theme_minimal() +
      theme(text = element_text(family = font_family, colour = "black"),
            axis.line.x.bottom = element_line(),
            axis.text = element_text(size = 14, color = "black"),
            axis.text.y = element_text(size = axis_text_y_size),
            axis.ticks.y = element_blank(),
            legend.key.height = unit(0.2, "cm"),
            legend.key.width = unit(1, "cm"),
            legend.position = legend_position,
            legend.justification = legend_just,
            legend.text = element_text(size = legend_size),
            panel.grid.major.x = element_line(colour = "gray90", size = 0.1),
            plot.caption = ggtext::element_markdown(face = "plain", size = legend_size),
            plot.title.position = title_position,
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = subtitle_size))

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
                                        "fehlende Werte",
                                        "valide Werte")) %>%
        dplyr::group_by(var, label, missings) %>%
        dplyr::summarise(value = sum(n)) %>%
        dplyr::group_by(var) %>%
        dplyr::mutate(perc = value/sum(value),
                      perc_text = ifelse(perc > 0,
                                         to_string_pct(perc, dec = dec, fac = 100),
                                         NA_character_),
                      perc = ifelse(missings == "fehlende Werte", perc * -1, perc),
                      x_text = ifelse(perc < 0,
                                      perc - na_graph_nudge,
                                      perc / 2))  %>%
        suppressMessages() %>%
        ggplot(data = ., aes(x = perc,
                             y = var,
                             fill = missings,
                             alpha = missings)) +
        geom_bar(stat = "identity",  width = bar_width) +
        geom_vline(xintercept = 0, color = "grey40", size = 0.75) +
        geom_text(aes(x = x_text,
                      color = missings,
                      label = perc_text),
                  family = font_family,
                  size = text_label_size,
                  show.legend = FALSE,
                  na.rm = TRUE) +
        scale_color_manual("", values = c("#b03060", "black")) +
        scale_fill_manual("", values = c("#b03060", "grey50")) +
        scale_alpha_manual("", values = c(0.8, 0.8)) +
        scale_x_continuous(expand = na_graph_expand,
                           limits = na_graph_limits,
                           breaks = seq(-1, 1, 0.2),
                           labels = scales::percent(abs(seq(-1, 1, 0.2)))) +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        lemon::coord_capped_cart(bottom = "both") +
        theme_minimal() +
        theme(text = element_text(family = font_family, colour = "black"),
              axis.line.x.bottom = element_line(),
              axis.text = element_text(size = 14, color = "black"),
              axis.ticks.y = element_blank(),
              legend.key.height = unit(0.2, "cm"),
              legend.key.width = unit(1, "cm"),
              legend.position = "bottom",
              legend.text = element_text(size = legend_size),
              panel.grid.major.x = element_line(colour = "gray90", size = 0.1),
              plot.title.position = "plot",
              plot.title = element_text(size = 18, face = "bold"),
              plot.subtitle = element_text(size = 16, margin = margin(0,0,20,0)),
              plot.caption = ggtext::element_markdown(face = "plain", size = legend_size),
              axis.text.y = element_blank())

      if (legend == "caption") {

        g_missing <- g_missing +
          labs(caption = glue::glue_collapse(x = glue::glue('<span style="color:{c("#b03060", "grey50")}">{c("fehlende Werte", "valide Werte")}</span>'),
                                             sep = "   <span style='color:grey80'>|</span>   ")) +
          theme(legend.position = "none")

      } else if (legend == "none") g_missing <- g_missing + theme(legend.position = "none")

      (g + g_missing + patchwork::plot_layout(ncol = 2, width = c(0.7, 0.3)))


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
                                 fill = value,
                                 alpha = alpha),
                             show.legend = FALSE) +
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
