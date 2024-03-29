% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_plots.R
\name{box_produce_plot}
\alias{box_produce_plot}
\title{Produces a set of box plots, allows for the inclusion of specific points}
\usage{
box_produce_plot(
  df,
  variable_fac,
  value_num,
  points_df = NULL,
  point_name = NULL,
  show_points = TRUE,
  xlab = NULL,
  ylab = NULL
)
}
\arguments{
\item{df}{the data frame with the data, already aggregated}

\item{variable_fac}{the column name representing the column variables}

\item{value_num}{the column name representing the values of the factors in
column `variable_fac`}

\item{points_df}{the data frame with the points you want on the box plot}

\item{point_name}{a label for the legend for your data frame}

\item{show_points}{TRUE if we wish to view all of the points. FALSE otherwise.
Defaults to TRUE.}

\item{xlab}{a label for the x axis otherwise defaults to variable_fac}

\item{ylab}{a label for the y axis otherwise defaults to value_num}
}
\value{
plot_obj a plot object
}
\description{
Produces a set of box plots, allows for the inclusion of specific points
}
