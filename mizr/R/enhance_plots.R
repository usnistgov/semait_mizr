mizr_set_theme <- function(base_size = 13) {
  #' Set ggplot theme to default mizr theme
  #' Sets the theme to the default mizr theme
  #' The theme can be further updated with:
  #' old_theme <- theme_set(theme_get() + <updates>)
  #' @return old_theme the old_theme before the theme change, as is consistent
  #' with ggplot themees
  old_theme <- theme_set(theme_bw(base_size = base_size, base_family = "sans") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0, "lines"),
      panel.border = element_blank(),
      strip.background = element_rect(
        fill = "grey90",
        colour = "#000000", size = 0.6
      ),
      strip.text = element_text(size = rel(1.0)),
      plot.title = element_text(
        size = rel(1.4), face = "bold",
        lineheight = 1.2
      )
    ))
  return(old_theme)
}

mizr_make_formula <- function(effects_var_vec, response_var, interaction_depth = 1) {
  #' Construct a formula string of response_var ~ effects_var_vec, interaction depth specified
  #'
  #' Constructs a forumla string of response_var ~ effects_var_vec,
  #' which includes all of the variables
  #' and all of the couplings (interactions) of all combinations all the way down to the
  #' interaction depth specified
  #' @param effects_var_vec the vector of effect variables to make the formula for
  #' @param response_var the response variable for the formula
  #' @param interaction_depth the desired depth of interactions to compute in the formula. The
  #' default is 1, which only computes the effects
  #' @return formula_str, a string of the desired formula

  # Construct formula from vectors
  formula_combinations <- t(combn(effects_var_vec, interaction_depth))
  formula_str <- paste0(response_var, " ~ ")

  first_var <- TRUE
  for (i in seq_len(nrow(formula_combinations))) {
    if (first_var) {
      first_var <- FALSE
    } else {
      formula_str <- formula_str <- paste0(formula_str, " + ")
    }
    formula_row <- formula_combinations[i, ]
    first_elt <- TRUE
    for (j in seq_len(length(formula_row))) {
      if (first_elt) {
        first_elt <- FALSE
      } else {
        formula_str <- paste0(formula_str, " * ")
      }
      formula_str <- paste0(formula_str, formula_row[j])
    }
  }
  out_formula <- formula(formula_str)
  return(out_formula)
}

eh_x_label <- function(plot_obj, xlabel) {
  #' Replaces x-axis label of a plot
  #' @param plot_obj the current plot object
  #' @param xlabel the desired x axis label text
  #' @return plot_obj a plot object with the modified x-axis label

  return(plot_obj + xlab(xlabel))
}

eh_y_label <- function(plot_obj, ylabel) {
  #' Replaces y-axis label of a plot
  #' @param plot_obj the current plot object
  #' @param ylabel the desired y axis label text
  #' @return plot_obj a plot object with the modified y-axis label

  return(plot_obj + ylab(ylabel))
}

eh_title <- function(plot_obj, titlestr) {
  #' Replaces title of a plot.
  #' Title can contain new lines with escape characters for multi-line plots.
  #' @param plot_obj the current plot object
  #' @param titlestr the desired title text
  #' @return plot_obj a plot object with the modified title

  return(plot_obj + ggtitle(titlestr))
}

eh_position_legend <- function(plot_obj, position) {
  #' Customizes the position of the legend
  #' This replaces the "outside" argument in plots that customizes legend positioning
  #' @param plot_obj the current plot object
  #' @param position The position of the legend, conforming with ggplot2
  #' specifications for position.
  #' It can be a word like "bottom" or c(xpos,ypos) vector from the bottom-right corner of the plot
  #' @return plot_obj a plot object with the repositioned legend

  return(plot_obj + theme(legend.position = position))
}

eh_window_limits <- function(plot_obj, xlim_vec, ylim_vec) {
  #' Zoom the plot to the desired x and y axis limits
  #'
  #' It uses coord_cartesian() from ggplot2 so that data outside the window is not
  #' thrown out (which actually influences how
  #' boxplots can look)
  #' @param plot_obj the current plot object
  #' @param xlim the x-axis limits as (xmin, xmax)
  #' @param ylim the y-axis limits as (ymin, ymax)
  #' @return a plot_object with zoomed-in limits.

  return(plot_obj + coord_cartesian(xlim = xlim_vec, ylim = ylim_vec))
}


get_x_label <- function(plot_obj) {
  #' Returns the current plot x-axis label as a string
  #'
  #' X-Axis label return used for enhancing x-axes labels
  #' @param plot_obj the current plot object
  #' @return x_str the x-axis label of the plot as a string.

  return(plot_obj$labels$x)
}

get_y_label <- function(plot_obj) {
  #' Returns the current plot y-axis label as a string
  #'
  #' Y-Axis label return used for enhancing y-axes labels
  #' @param plot_obj the current plot object
  #' @return y_str the y-axis label of the plot as a string.

  return(plot_obj$labels$y)
}

get_title <- function(plot_obj) {
  #' Returns the current plot title as a string
  #'
  #' Title return used for testing plots.
  #' @param plot_obj the current plot object
  #' @return title_str the title of the plot as a string.

  return(plot_obj$labels$title)
}

get_plot_data <- function(plot_obj) {
  #' Returns the current plot data frame as a data frame object
  #'
  #' Return of plot data used for additional plotting. Data frames may have extra rows and columns
  #' needed for plotting and plot aesthetics.
  #' @param plot_obj the current plot object
  #' @return plot_df the data frame used to produce the plot.

  return(plot_obj$data)
}
