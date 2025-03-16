## code to prepare `data-raw/bacteremia_mortality.csv` dataset goes here

library(tidyverse)

bacteremia_mortality <- read_csv('data-raw/bacteremia_mortality.csv',
                                 col_types = cols(
                                   hospital = col_character(),
                                   month = col_date(format = ""),
                                   deaths = col_integer(),
                                   cases = col_integer()
                                 )) |>
  as.data.frame()

usethis::use_data(bacteremia_mortality, overwrite = TRUE)
