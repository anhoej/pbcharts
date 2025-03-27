## code to prepare `data-raw/bacteremia_mortality.csv` dataset goes here

library(tidyverse)

hba1c <- read_csv('data-raw/diabetes_hba1c.csv',
                  comment = '#',
                  col_types = cols(
                    month = col_date(format = ""),
                    avg_hba1c = col_double(),
                    n = col_double()
                  )) |>
  as.data.frame()

usethis::use_data(hba1c, overwrite = TRUE)
