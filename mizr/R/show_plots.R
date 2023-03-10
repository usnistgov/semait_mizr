view_plot <- function(plot_obj) {
  #' Show plot
  #' @param plot_obj the plot object to plot
  #' @return plot_obj the same objected plotted so that it can be piped to a save_plot method

  return(plot_obj)
}

save_plot <- function(plot_obj, filepath, format = "png", width, height, units = "in") {
  #' Exports the plot as an image file
  #' @param plot_object the plot object to save
  #' @param filepath the file path to save the file to. Path is assumed to exist.
  #' @param format the image format to save as. It is preferred that the extension suggested here
  #' matches the extension of the filename in the filepath variable
  #' @param width the width to save the image as
  #' @param height the height to save the image as
  #' @param units the units the width and height are represented in. Inches ("in") by default.
  #' @return (nothing returned)

  ggsave(
    plot = plot_obj, filename = filepath, device = format, width = width,
    height = height, units = units
  )
}
