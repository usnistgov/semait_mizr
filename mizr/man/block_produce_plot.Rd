% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/block_plots.R
\name{block_produce_plot}
\alias{block_produce_plot}
\title{Produces a block plot}
\usage{
block_produce_plot(
  block_df,
  block_box_df,
  x_var_vec,
  treatment_var,
  response_var,
  value_text_size = rel(2)
)
}
\arguments{
\item{block_df}{the data frame after computations}

\item{block_box_df}{the data frame for each of the block rectangle coordinates.}

\item{x_var_vec}{the vector of columns to display on the x-axis.
Multiple factors can be combined to enumerate out the different values.}

\item{treatment_var}{the blocking factor or effect to plot separately as different symbols}

\item{response_var}{the response column to plot on the y axis.}

\item{value_text_size}{the size of the text within the plot. Often specified
as rel(Y) or a relative text size}
}
\value{
plot_obj a plot object with this plot to display
}
\description{
Produces a block plot
}
