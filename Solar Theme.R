library(ggplot2)
library(ggfx)

#' Custom ggplot2 Theme with Solar System Colors
#'
#' A dark space-themed ggplot2 theme with planetary colors.
theme_solar <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial", size = 15),
      plot.title = ggplot2::element_text(size = 24, face = "bold", color = "white"),
      axis.text = ggplot2::element_text(size = 18, color = "white"),
      axis.title = ggplot2::element_text(size = 18, color = "white"),
      panel.grid.major = ggplot2::element_line(color = "gray80"),
      panel.grid.minor = ggplot2::element_line(color = "gray90"),
      panel.background = ggplot2::element_rect(fill = "black", color = NA),
      plot.background = ggplot2::element_rect(fill = "black", color = NA),
      legend.background = ggplot2::element_rect(fill = "black"),
      legend.text = ggplot2::element_text(color = "white"),
      legend.title = ggplot2::element_text(color = "white")
    )
}

#' Solar System Color Palette
#'
#' A named vector of colors representing planets and space.
solar_system_palette <- c(
  "Mercury"     = "#8C8C8C",
  "Venus"       = "#E3C16F",
  "Earth Blue"  = "#2D68C4",
  "Earth Green" = "#3CB371",
  "Mars"        = "#D14A28",
  "Jupiter"     = "#C98C5A",
  "Saturn"      = "#E5C37F",
  "Uranus"      = "#7EB6FF",
  "Neptune"     = "#3759A3",
  "Sun"         = "#FFD700",
  "Space"       = "#000000"
)

#' Solar System Color Scale
#'
#' A ggplot2 scale for using the solar system colors.
scale_color_solar_system <- function() {
  ggplot2::scale_color_manual(values = solar_system_palette)
}

geom_glow <- function(geom_func, 
                      mapping = NULL, data = NULL,
                      color_main = "white", color_glow = "white",
                      size_main = 1.5, size_glow = 3,
                      alpha_glow = 0.2, ...) {
  list(
    do.call(geom_func, c(list(mapping = mapping, data = data,
                              size = size_glow, color = color_glow,
                              alpha = alpha_glow), list(...))),
    do.call(geom_func, c(list(mapping = mapping, data = data,
                              size = size_main, color = color_main), list(...)))
  )
}

#' Glowing line layer wrapper
#'
#' @param ... Arguments passed to geom_line()
#' @param glow_colour Colour of the glow
#' @param sigma Softness of the glow (higher = blurrier)
#' @param expand Glow spread amount
#'
#' @return A ggplot2 layer with outer glow applied
#' @export
geom_glow_line <- function(..., glow_colour = "white", sigma = 3, expand = 2) {
  ggfx::with_outer_glow(
    ggplot2::geom_line(...),
    colour = glow_colour,
    sigma = sigma,
    expand = expand
  )
}

#' Glowing point layer wrapper
#' @export
geom_glow_point <- function(..., glow_colour = "white", sigma = 2, expand = 1) {
  ggfx::with_outer_glow(
    ggplot2::geom_point(...),
    colour = glow_colour,
    sigma = sigma,
    expand = expand
  )
}

#' Custom color scale for solar palette
#' @export
scale_color_solar <- function(...) {
  ggplot2::scale_color_manual(values = your_custom_palette(), ...)
}
