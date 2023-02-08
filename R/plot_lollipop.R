#' Plots a Lollipop Plot
#'
#' Plots lollipop plots with or without grouping.
#' @param data Data frame
#' @param x variable names, must be a symbol or character vector
#' @param y variable names, must be a symbol or character vector
#' @param group group (optional)
#' @return A-ggplot()-object
#' @examples
#' plot_lollipop(data = iris, vars = setosa) ;
#' @export

plot_lollipop <- function(data,
                          x,
                          y,
                          group = NULL,
                          text,
                          group_color = NULL,
                          nudge_x = 0.05,
                          alpha = 0.6,
                          limits = NULL,
                          xlab = NULL,
                          point_color = "#e36c33",
                          point_size = 4,
                          segment_color = "#3e6487",
                          fontface = "plain",
                          flip = FALSE,
                          title = NULL,
                          subtitle = NULL,
                          legend = "standard",
                          legend_size = 12,
                          font_family = "Calibri",
                          axis_text_size = 12,
                          expand = expansion(mult = 0, add = 0),
                          scale_x_labels = scales::percent_format(scale = 100,
                                                                  big.mark = ".",
                                                                  decimal.mark = ",")) {

  nudge_x <- (max(pull(data, {{x}}))) * nudge_x

  limits <- c(0, (max(pull(data, {{x}})) + nudge_x) * 1.02)

  if (legend %nin% c("standard", "caption", "none")) {

    stop("legend must take values 'standard', 'caption' or 'none'")

  }

  group_enquo <- enquo(group)

  if (!quo_is_null(group_enquo)) {

    g <- ggplot(data = data) +
      geom_linerange(aes(xmin = 0,
                         xmax = {{x}},
                         y = {{y}},
                         color = {{group}},
                         group = {{group}}),
                     size = 1.5,
                     alpha = alpha,
                     position = position_dodge(width = 0.9)) +
      geom_point(aes(x = {{x}},
                     y = {{y}},
                     color = {{group}},
                     group = {{group}}),
                 size = point_size,
                 position = position_dodge(width = 0.9))


  } else {

    g <- ggplot(data = data) +
      geom_linerange(aes(xmin = 0,
                         xmax = {{x}},
                         y = {{y}}),
                     color = segment_color,
                     size = 1.5,
                     alpha = alpha) +
      geom_point(aes(x = {{x}},
                     y = {{y}}),
                 size = point_size,
                 color = point_color)

  }

  g <- g +
    geom_text(aes(x = {{x}} + nudge_x,
                  y = {{y}},
                  group = {{group}},
                  label = {{text}}),
              fontface = fontface,
              family = font_family,
              position = position_dodge(width = 0.9),
              color = "black",
              size = 4) +
    coord_capped_cart(bottom = "both") +
    scale_x_continuous(limits = limits,
                       expand = expand,
                       labels = scale_x_labels) +
    xlab(xlab) +
    ylab(NULL) +
    labs(title = title,
         subtitle = subtitle) +
    theme_minimal() +
    theme(text = element_text(family = font_family, colour = "black"),
          legend.key.height = unit(0.2, "cm"),
          legend.key.width = unit(1, "cm"),
          legend.position = "bottom",
          legend.text = element_text(size = legend_size),
          panel.grid.major.x = element_line(colour = "gray90", size = 0.01),
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

    if (is.null(group_color)) {

      g <- g +
        scale_color_ordinal("")

      group_color <- ggplot_build(g)
      group_color <- unique(group_color$data[[2]]["colour"])[[1]]

    } else if (!is.null(group_color)) {

      g <- g +
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

  g

}
