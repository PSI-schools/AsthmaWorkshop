## code to prepare `MyPallette` dataset goes here

MyPallette <-  list(
  black = "#252525",
  grey = "#f0f0f0",
  red = "#BA2F2AFF",
  blue = "#2166ACFF",
  green = "#088158FF",
  col_pla = "#91bfdb",
  col_drug = "#fc8d59"
)

usethis::use_data(MyPallette, overwrite = TRUE, internal = TRUE)
