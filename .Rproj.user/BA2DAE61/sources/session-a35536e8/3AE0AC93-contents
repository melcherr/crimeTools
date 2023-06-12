#' Returns a density or ridge density plot
#'
#' Returns a density or ridge density plot
#' @param data Data frame
#' @param x variable names, must be a symbol or character vector
#' @param y variable names, must be a symbol or character vector
#' @param group group (optional)
#' @return A-ggplot()-object
#' @export

plot_density <- function(data,
                         x,
                         group = NULL,
                         area_limits = NULL,
                         ridge_density = FALSE,
                         xlab = NULL,
                         ylab = NULL,
                         title = NULL,
                         subtitle = NULL,
                         legend = "standard",
                         legend_title = NULL,
                         scale_x_breaks = waiver(),
                         scale_x_labels = waiver(),
                         scale_y_labels = waiver(),
                         scale = 1.5,
                         alpha = 1,
                         group_color = NULL) {

  if (legend %nin% c("standard", "caption", "none")) {

    stop("legend must take values 'standard', 'caption' or 'none'")

  }

  group_enquo <- enquo(group)

  if (!ridge_density) {

    if (quo_is_null(group_enquo)) {

      g <- data %>%
        ggplot(data = .) +
        geom_density(aes(x = {{x}}),
                     fill = '#829cb2',
                     alpha = alpha,
                     color = '#3e6487')

    } else {

      data <- data %>% mutate({{group}} := sjlabelled::as_label({{group}},
                                                                drop.na = TRUE,
                                                                drop.levels = TRUE))

      g <- data %>%
        ggplot(data = .) +
        geom_density(aes(x = {{x}},
                         group ={{group}},
                         color = {{group}},
                         fill = {{group}}),
                     alpha = alpha)

    }

    g <- g +
      geom_vline(xintercept = area_limits[[1]],
                 color = "darkred",
                 linetype = "dashed") +
      scale_x_continuous(breaks = scale_x_breaks,
                         labels = scale_x_labels) +
      scale_y_continuous(labels = scale_y_labels) +
      crimeTools:::theme_crimeTools() +
      theme(axis.line.x.bottom = element_line(),
            axis.line.y.left = element_line(),
            axis.ticks.y.left = element_line(),
            axis.text.y.left = element_text(size = 16),
            axis.title = element_text(size = 16)) +
      lemon::coord_capped_cart(bottom = "both", left = "both") +
      xlab(xlab) +
      ylab(ylab) +
      labs(title = title, subtitle = subtitle) +
      guides(color = guide_legend(byrow = TRUE),
             fill = guide_legend(byrow = TRUE))

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

      labels <- levels(pull(data, {{group}}))

      g <- g +
        labs(caption = glue::glue_collapse(x = glue::glue('<span style="color:{group_color}">{labels}</span>'),
                                           sep = "   <span style='color:grey80'>|</span>   ")) +
        theme(legend.position = "none")

    } else if (legend == "none")  g <- g + theme(legend.position = "none")


    if (!is.null(area_limits) & quo_is_null(group_enquo)) {

      area_prop <-
        data %>%
        dplyr::mutate(in_area = dplyr::if_else(dplyr::between({{x}},
                                                              area_limits[[1]],
                                                              area_limits[[2]]),
                                               1,
                                               0,
                                               NA_real_)) %>%
        dplyr::pull(in_area) %>%
        sjmisc::frq(show.na = FALSE) %>%
        as.data.frame() %>%
        dplyr::filter(val == 1) %>%
        dplyr::pull(raw.prc) %>%
        sprintf("%.1f", .) %>%
        stringr::str_replace("\\.", ",") %>%
        paste0(., "%")

      g_build <- ggplot_build(g)
      g_build <- g_build$data[[1]]
      g_build <- g_build[between(g_build$x,
                                 left = area_limits[[1]],
                                 right = area_limits[[2]]),]

      g +
        geom_area(data = g_build, aes(x = x, y = y),
                  fill = "#f6d2c1",
                  color = "#e36c33") +
        geom_text(aes(x = mean(area_limits),
                      y = 0,
                      label = area_prop),
                  nudge_y = 0.025,
                  size = 5)

    } else g

  } else {

    if (is.null(group_color)) group_color <- c('#3e6487', "#e36c33")

    data <- data %>% mutate({{group}} := sjlabelled::as_label({{group}},
                                                              drop.na = TRUE,
                                                              drop.levels = TRUE))

    ggplot(data = data,
           aes(x = {{x}},
               y = {{group}},
               group = {{group}},
               fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient(scale = scale,
                                             show.legend = FALSE,
                                             na.rm = TRUE) +
      geom_vline(xintercept = area_limits[[1]],
                 color = "darkred",
                 linetype = "dashed") +
      scale_fill_gradientn(colours = group_color) +
      scale_x_continuous(breaks = scale_x_breaks,
                         labels = scale_x_labels) +
      crimeTools:::theme_crimeTools() +
      theme(axis.line.x.bottom = element_line(),
            axis.text = element_text(size = 16),
            axis.text.y.left = element_text(size = 16),
            axis.title = element_text(size = 16)) +
      lemon::coord_capped_cart(bottom = "both") +
      ylab(ylab) +
      xlab(xlab)

  }

}
