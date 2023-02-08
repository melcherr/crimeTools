plot_bar <- function(data,
                     y,
                     x,
                     xmin = NULL,
                     xmax = NULL,
                     group = NULL,
                     text,
                     width = 0.75,
                     bar_color = "#e36c33",
                     group_bar_color = NULL,
                     crossbar_color = "#3e6487",
                     conf_color = "black",
                     show_confint = TRUE,
                     alpha = 0.6,
                     flip = FALSE,
                     title = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     nudge_x = 0.035,
                     axis_text_size = 12,
                     legend = "standard",
                     legend_size = 12,
                     limits = NULL,
                     font_family = "Calibri",
                     expand = expansion(mult = 0, add = 0),
                     scale_x_labels = scales::percent_format(scale = 100,
                                                             big.mark = ".",
                                                             decimal.mark = ",")) {

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
               position = position_dodge(width = 0.9)) +
      geom_crossbar(aes(y = {{y}},
                        xmin = {{x}},
                        xmax = {{x}},
                        color = {{group}},
                        group = {{group}},
                        x = {{x}}),
                    width = width,
                    fatten = 2.5,
                    position = position_dodge(width = 0.9),
                    show.legend = FALSE)

  } else {

    g <- ggplot(data = data) +
      geom_bar(aes(y = {{y}},
                   x = {{x}}),
               stat = "identity",
               alpha = alpha,
               width = width,
               fill = bar_color,
               position = position_dodge(width = 0.9)) +
      geom_crossbar(aes(y = {{y}},
                        xmin = {{x}},
                        xmax = {{x}},
                        x = {{x}}),
                    width = width,
                    fatten = 2.5,
                    color = crossbar_color,
                    position = position_dodge(width = 0.9),
                    show.legend = FALSE)

  }


  g <- g +
    coord_capped_cart(bottom = "both") +
    xlab(xlab) + ylab(NULL) +
    labs(title = title,
         subtitle = subtitle) +
    theme_minimal() +
    theme(text = element_text(family = font_family,
                              colour = "black"),
          legend.key.height = unit(0.2, "cm"),
          legend.key.width = unit(1, "cm"),
          legend.position = "bottom",
          legend.text = element_text(size = legend_size),
          plot.title.position = "plot",
          plot.caption = element_markdown(face = "plain", size = legend_size),
          plot.margin = margin(0, 30, 0, 0),
          plot.subtitle = element_markdown(size = 16),
          plot.title = element_text(size = 18,
                                    face = "bold"),
          strip.text = element_text(size = 16,
                                    face = "bold",
                                    margin = margin(0, 0, 20, 0)),
          axis.text = element_text(size = axis_text_size,
                                   colour = "black"),
          axis.line.x.bottom = element_line(color = "gray25"),
          axis.ticks.x.bottom = element_line(color = "gray25"))

  if (flip) {

    g <- g +
      coord_capped_flip(left = "both") +
      theme(axis.line.x.bottom = element_blank(),
            axis.ticks.x.bottom = element_blank(),
            axis.line.y.left = element_line(color = "gray25"),
            axis.ticks.y.left = element_line(color = "gray25"))

  }

  if (!quo_is_null(group_enquo)) {

    if (is.null(group_bar_color)) {

      g <- g +
        scale_fill_ordinal("") +
        scale_color_ordinal("")

      group_bar_color <- ggplot_build(g)
      group_bar_color <- unique(group_bar_color$data[[1]]["fill"])[[1]]

    } else if (!is.null(group_bar_color)) {

      g <- g +
        scale_fill_manual("", values = group_bar_color) +
        scale_color_manual("", values = group_bar_color)

    }

    if (legend == "caption") {

      labels <- unique(g$plot$data$group_value)

      g <- g +
        labs(caption = glue::glue_collapse(x = glue::glue('<span style="color:{group_bar_color}">{labels}</span>'),
                                           sep = "   <span style='color:grey80'>|</span>   ")) +
        theme(legend.position = "none")

    } else if (legend == "none")  g <- g + theme(legend.position = "none")

  }

  if ((!quo_is_null(quo(xmin)) & !quo_is_null(quo(xmax))) & show_confint) {

    nudge_x <- max(pull(data, {{xmax}})) * nudge_x

    g <- g +
      geom_errorbar(aes(y = {{y}},
                        xmin = {{xmin}},
                        xmax = {{xmax}},
                        group = {{group}}),
                    width = 0.15,
                    color = conf_color,
                    position = position_dodge(width = 0.9),
                    show.legend = FALSE) +
      geom_text(aes(y = {{y}},
                    x = {{xmax}} + nudge_x,
                    group = {{group}},
                    label = {{text}}),
                position = position_dodge(width = 0.9),
                color = "black",
                family = font_family)

    if (is.null(limits)) limits <- c(0, (max(pull(data, {{xmax}})) + nudge_x) * 1.02)

    g + scale_x_continuous(limits = limits,
                           expand = expand,
                           labels = scale_x_labels)
  }

  else {

    if (is.null(limits)) limits <- c(0, (max(pull(data, {{x}})) + nudge_x) * 1.02)

    g <- g + scale_x_continuous(limits = limits,
                                expand = expand,
                                labels = scale_x_labels)

    nudge_x <- max(pull(data, {{x}})) * nudge_x

    g + geom_text(aes(y = {{y}},
                      x = {{x}} + nudge_x,
                      group = {{group}},
                      label = {{text}}),
                  position = position_dodge(width = 0.9),
                  color = "black",
                  family = font_family)

  }

}
