library(tidyverse)

ontime_ct <- read_csv('data-raw/ontime_ct.csv',
                      col_types = cols(
                        month = col_date(format = ""),
                        ontime = col_integer(),
                        cases = col_integer()
                      ),
                      comment = '#') |>
  as.data.frame()

usethis::use_data(ontime_ct, overwrite = TRUE)
