#' Standardize or Normalize Numeric Vectors
#'
#' This function transforms a numeric vector by either standardizing
#' (centering and dividing by standard deviation) or normalizing
#' (scaling to a range from 0 to a specified maximum).
#'
#' @param variable A numeric vector to be standardized or normalized.
#' @param type A character string indicating the type of transformation.
#'   Must be either `"divide_by_sd"` (subtract mean and divide by standard deviation)
#'   or `"normalize"` (rescale to the range \[0, 1\] or a multiple thereof).
#'   Defaults to `"divide_by_sd"`.
#' @param times A numeric scalar controlling the scaling intensity.
#'   For `type = "divide_by_sd"`, the standardized values are divided by
#'   `times * sd(variable)`. For `type = "normalize"`, the result is multiplied
#'   by `times` if `zero = TRUE`, or rescaled to \[1, times\] if `zero = FALSE`.
#'   Defaults to `2`.
#' @param zero Logical. If `TRUE`, normalized values are scaled between `0` and `times`.
#'   If `FALSE`, normalized values are scaled between `1` and `times`.
#'   Ignored when `type = "divide_by_sd"`. Defaults to `TRUE`.
#'
#' @return A numeric vector of the same length as `variable`, with standardized
#'   or normalized values.
#'
#' @details
#' The `"divide_by_sd"` option standardizes data by subtracting the mean
#' and dividing by the standard deviation (optionally scaled by `times`).
#'
#' The `"normalize"` option rescales data linearly to the range
#' \[0, 1\] (or another range depending on `times` and `zero`).
#'
#' @examples
#' x <- rnorm(100)
#'
#' # Standardize by mean and standard deviation
#' standardize(x)
#'
#' # Normalize to range 0–2
#' standardize(x, type = "normalize")
#'
#' # Normalize to range 1–3
#' standardize(x, type = "normalize", times = 3, zero = FALSE)
#'
#' @export

standardize <- function(variable, type = "divide_by_sd", times = 2, zero = TRUE) {

  if (!is.numeric(variable)) stop("'variable' must be numeric")

  if (type == "divide_by_sd") {

    mean_val <- mean(variable, na.rm = TRUE)
    sd_val <- sd(variable, na.rm = TRUE)

    if (sd_val == 0) stop("Standard deviation is zero; cannot divide by zero.")

    vec <- (variable - mean_val) / (times * sd_val)

    return(vec)

  }

  else if (type == "normalize") {

    min_val <- min(variable, na.rm = TRUE)
    max_val <- max(variable, na.rm = TRUE)

    if (max_val == min_val) stop("Range is zero; cannot normalize.")

    vec <- (variable - min_val) / (max_val - min_val)

    if (zero) {

      vec <- vec * times

    } else {

      vec <- (vec * (times - 1)) + 1

    }

    return(vec)

  }

  else stop("'type' must be either 'divide_by_sd' or 'normalize'")

}

