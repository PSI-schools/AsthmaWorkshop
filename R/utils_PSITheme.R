#' PSITheme
#' @description The PSI theme
#' @importFrom bslib bs_theme
#' @importFrom sass font_google
#' @noRd

psi_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000",
  primary = "#00638e",
  secondary = "#45C2D2",
  success = "#66b4eb",
  info = "#91c0d6",
  warning = "#FFF100",
  danger = "#e57a69",
  base_font = font_google("Questrial"),
  heading_font = font_google("Questrial"),
  code_font = "Chalkduster"
)
