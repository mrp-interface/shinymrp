# in R/zzz.R or similar
if (getRversion() >= "2.15.1") utils::globalVariables(".")

.onLoad <- function(libname, pkgname) {
  ggplot2::theme_set(
    ggplot2::theme_light(
      base_size   = 20
    ) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(hjust = 0.5),
      plot.caption = ggplot2::element_text(hjust = 0.5),
      plot.margin  = ggplot2::margin(1, 1, 1, 1, "cm")
    )
  )
}
