#' Plot Marginal Effects from a Model
#'
#' This function generates a marginal effect plot showing predicted values
#' from a fitted model across the range of a selected numeric variable.
#' It allows customization of prediction scale, standardization, labels, colors,
#' and axis appearance. Optionally, a rug plot of observed values can be added.
#'
#' @param model A fitted model object (e.g., from `glm()`, `lm()`, or similar)
#'   that contains a `model` data frame element.
#' @param var The numeric variable for which to plot marginal effects. Must be
#'   present in the model's data.
#' @param steps Numeric; the increment size used to create a sequence of values
#'   for predictions. Default is `0.01`.
#' @param type Character; the prediction type passed to
#'   `calculate_predicted_values()`. Default is `"link"`.
#' @param rug Logical; whether to add a rug plot of observed values along the
#'   x-axis. Default is `FALSE`.
#' @param standardize Logical; if `TRUE`, the x-variable is standardized using
#'   the `standardize()` function. Default is `TRUE`.
#' @param standardize_times Numeric; factor by which to multiply standardized
#'   values. Passed to `standardize()`. Default is `1`.
#' @param standardize_zero Logical; if `TRUE`, centers the standardized variable
#'   around zero. Default is `FALSE`.
#' @param labels Optional character vector providing labels for the different
#'   predicted variables (used in the legend).
#' @param colors Optional vector of colors to manually specify line colors.
#'   If `NULL`, default ggplot colors are used.
#' @param xlab Optional character string for the x-axis label. If `NULL`,
#'   the variable name is used.
#' @param ylab Character string for the y-axis label. Default is
#'   `"Predicted Values"`.
#' @param scale_y_limits Optional numeric vector of length 2 specifying the y-axis limits.
#' @param scale_x_limits Optional numeric vector of length 2 specifying the x-axis limits.
#' @param scale_x_breaks Axis breaks for the x-axis. Default is `waiver()`.
#' @param scale_x_labels Optional function or vector for x-axis labels.
#' @param guide Guide specification for the legend, passed to `guide_legend()`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Extracts the selected variable from the model data.
#'   \item Creates a sequence of values across the observed range.
#'   \item Uses `calculate_predicted_values()` to compute predictions.
#'   \item Optionally standardizes the variable.
#'   \item Plots predicted values with `ggplot2`, adding styling and
#'         optional rug plots.
#' }
#'
#' The mean of the x-variable is shown as a dashed red vertical line for reference.
#'
#' @return A `ggplot` object showing the marginal effects of the specified
#' variable on the predicted outcome.
#'
#' @seealso
#' \code{\link{calculate_predicted_values}} for generating predicted values,
#' \code{\link{standardize}} for variable standardization,
#' \code{\link[ggplot2]{ggplot}} for plot customization,
#' and \code{\link[lemon]{coord_capped_cart}} for capped coordinate axes.
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   library(dplyr)
#'
#'   # Example using a simple linear model
#'   model <- lm(mpg ~ wt + hp, data = mtcars)
#'
#'   # Plot marginal effects for 'wt'
#'   plot_predicted(model, wt)
#'
#'   # Plot with rug and customized steps
#'   plot_predicted(model, hp, steps = 0.05, rug = TRUE)
#' }
#'
#' @importFrom dplyr pull mutate tibble
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_vline geom_rug
#'   scale_color_manual scale_color_ordinal scale_x_continuous
#'   scale_y_continuous xlab ylab theme element_text element_line
#' @importFrom lemon coord_capped_cart
#' @importFrom rlang ensym as_name
#' @export

plot_predicted <- function(model,
                           var,
                           steps = 0.01,
                           type = "link",
                           rug = FALSE,
                           standardize = TRUE,
                           standardize_times = 1,
                           standardize_zero = FALSE,
                           labels = NULL,
                           colors = NULL,
                           xlab = NULL,
                           ylab = "Predicted Values",
                           scale_y_limits = NULL,
                           scale_x_limits = NULL,
                           scale_x_breaks = waiver(),
                           scale_x_labels = NULL,
                           guide = guide_legend(byrow = TRUE)) {

  # --- Extract the variable from the model data ---
  x <- model$model %>%
    dplyr::pull({{ var }})

  # Safety check
  if (!is.numeric(x)) stop("The selected variable must be numeric.")

  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)

  # --- Generate prediction data ---
  data <- dplyr::tibble({{ var }} := seq(min_x, max_x, by = steps)) %>%
    calculate_predicted_values(
      model = model,
      type = type,
      newdata = .,
      add_input = TRUE
    ) %>%
    tidyr::pivot_longer(cols = -{{ var }})

  # --- Apply labels if provided ---
  if (!is.null(labels)) {

    data <- data %>%
      dplyr::mutate(name = factor(name, labels = labels))

  }

  # --- Optionally standardize x variable ---
  if (standardize) {

    data <- data %>%
      dplyr::mutate({{ var }} := standardize(
        variable = {{ var }},
        times = standardize_times,
        zero = standardize_zero
      ))

  }

  # --- Base ggplot ---
  g <- ggplot(data, aes(x = {{ var }}, y = value, color = name, group = name)) +
    geom_line(linewidth = 1.2) +
    geom_vline(
      xintercept = mean(dplyr::pull(data, {{ var }}), na.rm = TRUE),
      color = "darkred",
      linetype = "dashed"
    )

  # --- Optional rug plot ---
  if (rug) {

    g <- g +
      geom_rug(
        data = model$model,
        mapping = aes(x = {{ var }}),
        alpha = 0.1
      )

  }

  # --- Color handling ---
  if (is.null(colors)) {
    g <- g + scale_color_ordinal(guide = guide)
  } else {
    g <- g + scale_color_manual("", values = colors, guide = guide)
  }

  # --- Final styling ---
  g +
    crimeTools:::theme_crimeTools() +
    theme(
      axis.title = element_text(size = 14),
      axis.line.y.left = element_line(),
      axis.ticks.y.left = element_line()
    ) +
    scale_y_continuous(limits = scale_y_limits) +
    scale_x_continuous(
      limits = scale_x_limits,
      breaks = scale_x_breaks,
      labels = scale_x_labels
    ) +
    xlab(xlab %||% rlang::as_name(rlang::ensym(var))) +
    ylab(ylab) +
    lemon::coord_capped_cart(left = "both", bottom = "both")

}

