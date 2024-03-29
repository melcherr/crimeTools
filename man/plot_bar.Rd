% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bar.R
\name{plot_bar}
\alias{plot_bar}
\title{Plot Bar Charts and Grouped Bar Charts with Confidence Intervals}
\usage{
plot_bar(
  data,
  y,
  x,
  xmin = NULL,
  xmax = NULL,
  group = NULL,
  text,
  width = 0.75,
  position_dodge_width = 0.9,
  bar_color = "#e36c33",
  group_color = NULL,
  crossbar_color = "#3e6487",
  conf_color = "black",
  show_confint = TRUE,
  alpha = 0.6,
  flip = FALSE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  nudge_x = 0.035,
  axis_text_size = 12,
  text_label_size = 4,
  text_label_align = 0.5,
  legend = "standard",
  legend_size = 12,
  limits = NULL,
  font_family = "",
  expand = expansion(mult = 0, add = 0),
  scale_x_labels = scales::percent_format(scale = 100, big.mark = ".", decimal.mark =
    ","),
  ...
)
}
\arguments{
\item{data}{Data frame
#' @param x variable names, must be a symbol or character vector}

\item{y}{variable names, must be a symbol or character vector}

\item{group}{group (optional)}
}
\value{
A-ggplot2::ggplot()-object
}
\description{
Plots bar charts and grouped bar charts with confidence intervals.
}
\examples{
plot_bar(data = iris, vars = setosa) ;
}
