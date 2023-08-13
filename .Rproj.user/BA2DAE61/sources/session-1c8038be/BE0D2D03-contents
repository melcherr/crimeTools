#' Returns an marginal effect plot
#'
#' Returns an marginal effect plot
#' @return A-ggplot()-object
#' @export


plot_predicted <- function(model,
                           var,
                           steps = 0.01,
                           type = "link",
                           labels = NULL,
                           colors = NULL,
                           xlab = NULL,
                           ylab = "Predicted Values",
                           scale_y_limits = NULL,
                           scale_x_limits = NULL,
                           scale_x_breaks = waiver(),
                           guide = guide_legend(byrow = TRUE)) {

  x <- model$model %>%
    dplyr::pull({{var}})

  min_x <- min(x)
  max_x <- max(x)

  data <- dplyr::tibble({{var}} := seq(min_x, max_x, steps))  %>%
    calculate_predicted_values(model = model,
                               type = type,
                               newdata = .,
                               add_input = TRUE) %>%
    tidyr::pivot_longer(cols = -{{var}})

  if (!is.null(labels)) {

    data <- data %>%
      dplyr::mutate(name = factor(name, labels = labels))

  }

  g <- data %>%
    ggplot() +
    geom_line(aes(x = {{var}},
                  y = value,
                  group = name,
                  color = name),
              linewidth = 1.2) +
    geom_vline(xintercept = 0,
               color = "darkred",
               linetype = "dashed") +
    geom_rug(data = model$model,
             aes(x = {{var}}),
             alpha = 0.1)

  if (is.null(colors)) {

    g <- g +
      scale_color_ordinal(guide = guide)

  } else {

    g <- g +
      scale_color_manual("",
                         values = colors,
                         guide = guide)


  }

  g +
    theme_crimeTools() +
    theme(axis.title = element_text(size = 14),
          axis.line.y.left = element_line(),
          axis.ticks.y.left = element_line()) +
    scale_y_continuous(limits = scale_y_limits) +
    scale_x_continuous(limits = scale_x_limits,
                       breaks = scale_x_breaks) +
    xlab(xlab) +
    ylab(ylab) +
    lemon::coord_capped_cart(left = "both",
                             bottom = "both")

}
