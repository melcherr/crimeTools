% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_predicted.R
\name{plot_predicted}
\alias{plot_predicted}
\title{Returns an marginal effect plot}
\usage{
plot_predicted(
  model,
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
  guide = guide_legend(byrow = TRUE)
)
}
\value{
A-ggplot()-object
}
\description{
Returns an marginal effect plot
}
