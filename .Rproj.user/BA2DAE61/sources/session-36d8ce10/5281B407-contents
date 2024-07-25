#' Returns an interpolated map
#'
#' Returns an interpolated map using inverse distance weighting
#' @param data spatial dataframe of type sf
#' @param location_data spatial dataframe of type sf
#' @param formula formula string
#' @return A-ggplot()-object
#' @export

plot_idw <- function(data,
                     location_data,
                     formula,
                     border_data = NULL,
                     low = "#002664",
                     high = "#F6D2C1",
                     contour = TRUE,
                     title = NULL,
                     subtitle = NULL,
                     legend_labels = scales::comma_format(accuracy = 0.1),
                     ...) {

  inverse_distances <- gstat::idw(as.formula(formula), data, location_data, ...)

  g <- ggplot() +
    stars::geom_stars(data = inverse_distances,
                      aes(fill = var1.pred,
                          x = x,
                          y = y)) +
    geom_sf(data = data, color = "black") +
    xlab(NULL) +
    ylab(NULL) +
    labs(title = title,
         subtitle = subtitle) +
    scale_fill_gradient("",
                        low = low,
                        high = high,
                        na.value = "transparent",
                        labels = legend_labels) +
    theme_void() +
    theme(legend.position = "right",
          legend.key.height = unit(3.5, "cm"),
          legend.key.width = unit(0.25, "cm"),
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 16),
          legend.text = element_text(size = 14))

  if (contour) {

    g <- g + stat_contour(data = dplyr::as_tibble(inverse_distances),
                          aes(z = var1.pred,
                              x = x,
                              y = y),
                          color = "black")

  }

  if (!is.null(border_data)) {

    border_data <- sf::st_cast(border_data, "MULTILINESTRING")

    g <- g + geom_sf(data = border_data,
                     color = "white")

  }

  g

}
