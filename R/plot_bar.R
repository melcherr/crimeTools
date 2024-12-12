#' Plot Bar Charts and Grouped Bar Charts with Confidence Intervals
#'
#' Plots bar charts and grouped bar charts with confidence intervals.
#' @param data Data frame
#' #' @param x variable names, must be a symbol or character vector
#' @param y variable names, must be a symbol or character vector
#' @param group group (optional)
#' @return A-ggplot2::ggplot()-object
#' @examples
#' plot_bar(data = iris, vars = setosa) ;
#' @export

plot_bar <- function(data,
                     y,
                     x,
                     xmin = NULL,
                     xmax = NULL,
                     group = NULL,
                     text,
                     width = 0.75,
                     position_dodge_width = 0.9,
                     bar_color = "#e36c33",
                     group_color = NULL,
                     crossbar_color = "#3e6487",
                     conf_color = "black",
                     show_confint = TRUE,
                     alpha = 0.6,
                     flip = FALSE,
                     title = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     nudge_x = 0.035,
                     axis_text_size = 12,
                     text_label_size = 4,
                     text_label_align = 0.5,
                     legend = "standard",
                     legend_size = 12,
                     limits = NULL,
                     font_family = "",
                     expand = expansion(mult = 0, add = 0),
                     scale_x_labels = scales::percent_format(scale = 100, big.mark = ".", decimal.mark = ","),
                     ...) {

  if (legend %nin% c("standard", "caption", "none")) {

    stop("legend must take values 'standard', 'caption' or 'none'")

  }

  group_enquo <- enquo(group)

  if (!quo_is_null(group_enquo)) {

    g <- ggplot(data = data) +
      geom_bar(aes(y = {{y}},
                   x = {{x}},
                   fill = {{group}},
                   group = {{group}}),
               stat = "identity",
               alpha = alpha,
               width = width,
               position = position_dodge(width = position_dodge_width)) +
      geom_crossbar(aes(y = {{y}},
                        xmin = {{x}},
                        xmax = {{x}},
                        color = {{group}},
                        group = {{group}},
                        x = {{x}}),
                    width = width,
                    fatten = 2.5,
                    position = position_dodge(width = position_dodge_width),
                    show.legend = FALSE)

  } else {

    g <- ggplot(data = data) +
      geom_bar(aes(y = {{y}},
                   x = {{x}}),
               stat = "identity",
               alpha = alpha,
               width = width,
               fill = bar_color,
               position = position_dodge(width = position_dodge_width)) +
      geom_crossbar(aes(y = {{y}},
                        xmin = {{x}},
                        xmax = {{x}},
                        x = {{x}}),
                    width = width,
                    fatten = 2.5,
                    color = crossbar_color,
                    position = position_dodge(width = position_dodge_width),
                    show.legend = FALSE)

  }


  g <- g +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(NULL) +
    labs(title = title,
         subtitle = subtitle) +
    lemon::coord_capped_cart(bottom = "both") +
    theme_crimeTools(...)


  if (flip) {

    g <- g +
      lemon::coord_capped_flip(left = "both") +
      theme(axis.line.x.bottom = element_blank(),
            axis.ticks.x.bottom = element_blank(),
            axis.line.y.left = element_line(),
            axis.ticks.y.left = element_line())

  }

  if (!quo_is_null(group_enquo)) {

    if (is.null(group_color)) {

      g <- g +
        scale_fill_ordinal("") +
        scale_color_ordinal("")

      group_color <- ggplot_build(g)
      group_color <- unique(group_color$data[[1]]["fill"])[[1]]

    } else if (!is.null(group_color)) {

      g <- g +
        scale_fill_manual("", values = group_color) +
        scale_color_manual("", values = group_color)

    }

    if (legend == "caption") {

      labels <- unique(g$plot$data$group_value)

      g <- g +
        labs(caption = glue::glue_collapse(x = glue::glue('<span style="color:{group_color}">{labels}</span>'),
                                           sep = "   <span style='color:grey80'>|</span>   ")) +
        theme(legend.position = "none")

    } else if (legend == "none")  g <- g + theme(legend.position = "none")

  }

  if ((!quo_is_null(quo(xmin)) & !quo_is_null(quo(xmax))) & show_confint) {

    nudge_x <- max(dplyr::pull(data, {{xmax}})) * nudge_x

    data <- data %>%
      dplyr::mutate(text_x = ifelse({{x}} < 0, {{xmin}} - nudge_x, {{xmax}} + nudge_x))


    g <- g +
      geom_errorbar(aes(y = {{y}},
                        xmin = {{xmin}},
                        xmax = {{xmax}},
                        group = {{group}}),
                    width = 0.15,
                    color = conf_color,
                    position = position_dodge(width = position_dodge_width),
                    show.legend = FALSE) +
      geom_text(data = data,
                aes(y = {{y}},
                    x = text_x,
                    group = {{group}},
                    label = {{text}}),
                position = position_dodge(width = position_dodge_width),
                color = "black",
                size = text_label_size,
                hjust = text_label_align,
                family = font_family)

    if (is.null(limits)) limits <- c(0, (max(dplyr::pull(data, {{xmax}})) + nudge_x) * 1.02)

    g + scale_x_continuous(limits = limits,
                           expand = expand,
                           labels = scale_x_labels)
  }

  else {

    if (is.null(limits)) limits <- c(0, (max(dplyr::pull(data, {{x}})) + nudge_x) * 1.02)

    g <- g + scale_x_continuous(limits = limits,
                                expand = expand,
                                labels = scale_x_labels)

    nudge_x <- max(dplyr::pull(data, {{xmax}})) * nudge_x

    data <- data %>%
      dplyr::mutate(text_x = ifelse({{x}} < 0, {{xmin}} - nudge_x, {{xmax}} + nudge_x))


    g + geom_text(data = data,
                  aes(y = {{y}},
                      x = text_x,
                      group = {{group}},
                      label = {{text}}),
                  position = position_dodge(width = position_dodge_width),
                  color = "black",
                  size = text_label_size,
                  hjust = text_label_align,
                  family = font_family)

  }

}
