library(ggplot2)

#' Custom ggplot2 Theme with Solar System Colors
#'
#' A dark space-themed ggplot2 theme with planetary colors.
solar <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial", size = 12),
      plot.title = ggplot2::element_text(size = 15, face = "bold", color = "white"),
      axis.text = ggplot2::element_text(size = 14, color = "white"),
      axis.title = ggplot2::element_text(size = 15, color = "white"),
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

geom_glow_line <- function(mapping = NULL, data = NULL, 
                           color_main = "white", color_glow = "white",
                           size_main = 1.5, size_glow = 3, alpha_glow = 0.2, ...) {
  list(
    geom_line(mapping = mapping, data = data, 
              size = size_glow, color = color_glow, alpha = alpha_glow, ...),
    geom_line(mapping = mapping, data = data, 
              size = size_main, color = color_main, ...)
  )
}
