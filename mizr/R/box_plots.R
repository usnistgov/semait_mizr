box_compute_df <- function(df, variable_fac, value_num,
                           points_df = NULL, point_name = NULL,
                           xlab = NULL, ylab = NULL) {
  return(df)
}

box_produce_plot <- function(df, variable_fac, value_num, points_df = NULL,
                             point_name = NULL, show_points = TRUE, xlab = NULL, ylab = NULL) {
  #' Produces a set of box plots, allows for the inclusion of specific points
  #' @param df the data frame with the data, already aggregated
  #' @param variable_fac the column name representing the column variables
  #' @param value_num the column name representing the values of the factors in
  #' column `variable_fac`
  #' @param points_df the data frame with the points you want on the box plot
  #' @param point_name a label for the legend for your data frame
  #' @param show_points TRUE if we wish to view all of the points. FALSE otherwise.
  #' Defaults to TRUE.
  #' @param xlab a label for the x axis otherwise defaults to variable_fac
  #' @param ylab a label for the y axis otherwise defaults to value_num
  #' @return plot_obj a plot object

  plot_obj <-
    ggplot(data = df) +
    geom_boxplot(aes_string(x = variable_fac, y = value_num))

  if (show_points) {
    plot_obj <- plot_obj + geom_point(aes_string(x = variable_fac, y = value_num))
  }

  if (!is.null(xlab) && !is.null(ylab)) {
    plot_obj <- plot_obj + labs(y = ylab, x = xlab)
  }

  if (!is.null(points_df)) {
    if (length(unique(df[[variable_fac]])) != nrow(points_df)) {
      stop("points vector is not proper length for box plot")
    } else {
      if (is.null(point_name)) {
        plot_obj <-
          plot_obj + geom_point(
            data = points_df,
            aes_string(
              x = variable_fac,
              y = value_num,
              colour = as.factor(1)
            )
          ) +
          scale_colour_manual(
            values = c(`1` = "red"),
            labels = c(""),
            name = ""
          ) +
          theme(axis.ticks.x = element_blank(), legend.position = "none")
      } else {
        plot_obj <-
          plot_obj + geom_point(
            data = points_df,
            aes_string(
              x = variable_fac,
              y = value_num,
              colour = as.factor(1)
            )
          ) +
          scale_colour_manual(
            values = c(`1` = "red"),
            labels = c(""),
            name = point_name
          ) +
          theme(axis.ticks.x = element_blank(), legend.position = "right")
      }
    }
  }


  return(plot_obj)
}
