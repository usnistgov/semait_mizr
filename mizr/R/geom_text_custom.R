# Taken and adapted from
# https://stackoverflow.com/questions/49965758/change-geom-texts-default-a-
# (continued) legend-to-label-string-itself
# Adapting code from http://felixfan.github.io/ggplot2-remove-grid-background-margin/
# to change the key to generate
# string symbols as shapes


get_custom_draw_key <- function(var_key_lvl, cols_vec) {
  #' Custom Geom Text Object draw key method
  #'
  #' Taken from https://stackoverflow.com/questions/49965758/change-geom-texts-default-a-
  #' (continued) legend-to-label-string-itself
  #' var_key = block_point_levels, (or some levels)
  #' cols = scales::hue_pal()(length(var_key))

  # sort as ggplot sorts these alphanumerically / or levels of factor
  # Works for factors and continuous variables
  custom_draw_key_fcn <- function(data, params, size, var_key = var_key_lvl, cols = cols_vec) {
    #' Custom Geom Text Object draw key method
    #'
    #' Taken from https://stackoverflow.com/questions/49965758/change-geom-texts-default-a-
    #' (continued) legend-to-label-string-itself
    #' var_key = block_point_levels, (or some levels)
    #' cols = scales::hue_pal()(length(var_key))

    # sort as ggplot sorts these alphanumerically / or levels of factor
    # Works for factors and continuous variables
    txt <- if (is.factor(var_key)) levels(var_key) else sort(var_key)
    txt <- txt[match(data$colour, cols)]

    textGrob(txt, 0.5, 0.5,
      just = "center",
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontfamily = data$family,
        fontface = data$fontface,
        fontsize = data$size * .pt
      )
    )
  }

  return(custom_draw_key_fcn)
}

get_custom_text_object <- function(var_key, cols = scales::hue_pal()(length(var_key))) {
  #' Get Custom geom_text object to have legends contain symbols in graph
  #'
  #' Adapted from https://stackoverflow.com/questions/49965758/change-geom-texts-default-a-
  #' (continued) legend-to-label-string-itself
  #' Produces a custom geom_text object that is used to enhance plots. Placed here to avoid
  #' the replication in individual plots.

  # Begin Exclude Linting
  geom_text_custom_obj <- GeomText
  # End Exclude Linting
  geom_text_custom_obj$draw_key <- get_custom_draw_key(var_key, cols)
  return(geom_text_custom_obj)
}

geom_text_custom <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", ..., parse = FALSE,
                             nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                             geom = geom_text_custom_obj) {
  #' Custom geom text custom rendering geom
  #'
  #' Taken from https://stackoverflow.com/questions/49965758/change-geom-texts-default-a-
  #' (continued) legend-to-label-string-itself
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.",
        call. = FALSE
      )
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer(
    data = data, mapping = mapping, stat = stat, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      parse = parse, check_overlap = check_overlap,
      na.rm = na.rm, ...
    )
  )
}
