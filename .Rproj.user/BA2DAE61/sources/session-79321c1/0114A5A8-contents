#' Returns a Coefficient Plot
#'
#' Returns a Coefficient Plot
#' @param data Data frame
#' @param x variable names, must be a symbol or character vector
#' @param y variable names, must be a symbol or character vector
#' @param group group (optional)
#' @return A-ggplot()-object
#' @export

plot_model <- function(data,
                       x,
                       y,
                       group = NULL,
                       ymin_90,
                       ymax_90,
                       ymin_95,
                       ymax_95,
                       ymin_99,
                       ymax_99,
                       facet_group_row = NULL,
                       patchwork_group = NULL,
                       shape_values = NULL,
                       shape_sizes = NULL,
                       conf_colors = c('#3e6487', '#829cb2', '#c0cdd8'),
                       strip_color = "#fbede6",
                       patchwork_color = "#f6d2c1",
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       breaks = 5,
                       ylab = "Regressionskoeffizienten",
                       xlab = "",
                       xlabels,
                       accuracy = 0.01,
                       ylim = NULL,
                       scales = "free_y",
                       space ="free",
                       guide_nrow = 3,
                       guide_ncol = 2,
                       legend_position = "bottom",
                       panel_spacing = 0.5,
                       font_family = "Arial",
                       axis_text_size = 13,
                       outer_text_size = 13,
                       ...) {

  facet_group_row <- rlang::enquo(facet_group_row)
  patchwork_group <- rlang::enquo(patchwork_group)

  if (!rlang::quo_is_null(patchwork_group)) {

    parameter <- as.list(match.call())
    parameter <- parameter[which(names(parameter) %nin% c("data", "patchwork_group", "title", "subtitle"))[-1]]

    group_labels <- dplyr::pull(data, {{patchwork_group}}) %>%
      unique()

    data_split <- data %>% dplyr::group_split({{patchwork_group}})

    proportion <- purrr::map_dbl(data_split, ~nrow(.x))

    g_wrap <-
      data_split %>%
      purrr::map2(.x = .,
                  .y = group_labels,
                  function(x, y) {

                    parameter <- parameter %>%
                      append(., list(data = x,
                                     title = y))

                    expr(plot_model(!!!parameter)) %>%
                      eval_tidy(.)

                  }) %>%
      purrr::map(.x = .,
                 ~.x +
                   theme(plot.title.position = "panel",
                         plot.title = ggtext::element_textbox_simple(
                           size = 16,
                           lineheight = 1,
                           hjust = 0,
                           fill = patchwork_color,
                           padding = margin(5, 5, 5, 5),
                           margin = margin(0, 0, 10, 0))))

    g_wrap[-length(group_labels)] <-
      g_wrap[-length(group_labels)] %>%
      purrr::map(.x = .,
                 ~.x +
                   theme(axis.title.x = element_blank(),
                         axis.text.x.bottom = element_blank(),
                         axis.ticks.x.bottom = element_blank(),
                         axis.line.x.bottom = element_line(color = "transparent")))

    g_wrap %>%
      patchwork::wrap_plots(ncol = 1,
                            nrow = length(group_labels),
                            heights = proportion/sum(proportion),
                            guides = "collect") +
      patchwork::plot_annotation(title = title,
                                 subtitle = subtitle,
                                 caption = caption,
                                 theme = theme(plot.title.position = "plot",
                                               plot.title = element_text(size = 18, face = "bold"),
                                               plot.subtitle = element_text(size = 16,
                                                                            margin = margin(0, 0, 20 , 0)),
                                               plot.caption = ggtext::element_textbox_simple(halign = 0,
                                                                                             margin = margin(-90,0,0,0),
                                                                                             padding = margin(5, 5, 5, 5),
                                                                                             lineheight = 1.35,
                                                                                             linetype = 1,
                                                                                             linewidth = 1.25,
                                                                                             size = 12,
                                                                                             hjust = 0,
                                                                                             maxwidth = grid::unit(6, "cm"),
                                                                                             box.color = "#f6d2c1",
                                                                                             fill = "#fbede6",
                                                                                             r = grid::unit(3, "pt")),
                                               plot.caption.position = "plot")) &
      theme(legend.position = legend_position)

  } else {

    g <- ggplot(data = data) +
      geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +
      geom_linerange(aes(ymax = {{ymax_99}},
                         ymin = {{ymin_99}},
                         x = {{x}},
                         color = "ci_99",
                         alpha = "ci_99",
                         group = {{group}}),
                     position = position_dodge2(width = 0.6, reverse = TRUE),
                     linewidth = 2,
                     key_glyph = "path") +
      geom_linerange(aes(x = {{x}},
                         ymin = {{ymin_95}},
                         ymax = {{ymax_95}},
                         color = "ci_95",
                         alpha = "ci_95",
                         group = {{group}}),
                     position = position_dodge2(width = 0.6, reverse = TRUE),
                     linewidth = 3,
                     key_glyph = "path") +
      geom_linerange(aes(x = {{x}},
                         ymin = {{ymin_90}},
                         ymax = {{ymax_90}},
                         color = "ci_90",
                         alpha = "ci_90",
                         group = {{group}}),
                     position = position_dodge2(width = 0.6, reverse = TRUE),
                     linewidth = 4,
                     key_glyph = "path") +
      geom_point(aes(x = {{x}}, y = {{y}},
                     shape = {{group}},
                     size = {{group}}),
                 position = position_dodge2(width = 0.6, reverse = TRUE),
                 fill = 'white',
                 color = "white",
                 size = 3) +
      scale_y_continuous(breaks = scales::pretty_breaks(breaks),
                         labels = scales::comma_format(accuracy = accuracy,
                                                       big.mark = ".",
                                                       decimal.mark = ",",
                                                       ...)) +
      scale_x_discrete("") +
      scale_color_manual("",
                         values = c("ci_90" = conf_colors[[1]],
                                    "ci_95" = conf_colors[[2]],
                                    "ci_99" = conf_colors[[3]]),
                         labels = c("90%-Konfidenzintervall",
                                    "95%-Konfidenzintervall",
                                    "99%-Konfidenzintervall"),
                         guide = guide_legend(ncol = guide_ncol,
                                              nrow = guide_nrow)) +
      scale_alpha_manual("", values = c("ci_90" = 0.6,
                                        "ci_95" = 0.6,
                                        "ci_99" = 0.8),
                         labels = c("90%-Konfidenzintervall",
                                    "95%-Konfidenzintervall",
                                    "99%-Konfidenzintervall"),
                         guide = guide_legend(ncol = guide_ncol,
                                              nrow = guide_nrow)) +
      scale_size_manual("",
                        values = shape_sizes,
                        guide = guide_legend(order = 1,
                                             ncol = guide_ncol,
                                             nrow = guide_nrow)) +
      scale_shape_manual("",
                         values = shape_values,
                         guide = guide_legend(order = 1,
                                              ncol = guide_ncol,
                                              nrow = guide_nrow,
                                              override.aes = list(color = conf_colors[[1]],
                                                                  fill = "white",
                                                                  size = shape_sizes))) +
      theme_tufte() +
      theme(text = element_text(family = font_family),
            axis.text.x.bottom = ggtext::element_markdown(color = "black",
                                                  size = axis_text_size),
            axis.text.y.left = ggtext::element_markdown(color = "black",
                                                size = axis_text_size),
            axis.ticks.y = element_blank(),
            axis.line = element_line(),
            axis.line.y.left = element_blank(),
            axis.title = element_text(size = outer_text_size),
            plot.title = element_text(size = 18, face = "bold"),
            panel.spacing = unit(panel_spacing, "lines"),
            plot.subtitle = element_text(size = 16),
            plot.title.position = "plot",
            plot.margin = margin(0, 0 ,0 ,0),
            panel.border = element_blank(),
            panel.grid.major.x = element_line(linewidth = 0.1, color = "gray90"),
            panel.grid.major.y = element_line(linewidth = 0.1, color = "gray90"),
            legend.position = legend_position,
            legend.text = element_text(size = outer_text_size),
            strip.text = ggtext::element_markdown(size = outer_text_size,
                                                  hjust = 0),
            strip.background = element_rect(fill = strip_color,
                                            colour = "transparent")) +
      lemon::coord_capped_flip(ylim = ylim, bottom ='both') +
      xlab(xlab) + ylab(ylab) +
      labs(title = title,
           subtitle = subtitle)

    if (!rlang::quo_is_null(facet_group_row)) {

      g + ggforce::facet_col(vars({{facet_group_row}}),
                             space = "free",
                             scales = scales)
    }

    else return(g)

  }

}
