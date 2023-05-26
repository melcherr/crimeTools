#' Standardizes and normalizes numeric vectors.
#'
#' Returns a vector of standardized values.
#' @param variable a numeric vector
#' @param type, takes the values "divide_by_sd" and "normalize"
#' @return a numeric vector
#' @examples
#' x <- rnorm(100)
#' standardize(x) ;
#' @export


standardize <- function(variable, type = "divide_by_sd", times = 2){

  if (type == "divide_by_sd") {

    (variable - mean(variable, na.rm = TRUE)) / (times * sd(variable, na.rm = TRUE))

  } else if (type == "normalize") {

    min <- min(variable , na.rm = TRUE)
    max <- max(variable , na.rm = TRUE)

    if (min > 0) {

      (variable - min) / ( max - min)

    }

    else {

      (variable + abs(min) ) / (max + abs(min) )

    }

  } else stop("'type' must take the values 'divide_by_sd' or 'normalize'" )

}
