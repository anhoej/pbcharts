library(tidyverse)

csection <- read_csv('data-raw/csection_delay.csv',
                     col_types = cols(
                       datetime = col_datetime(format = ""),
                       month = col_date(format = ""),
                       delay = col_integer()),
                       comment = '#') |>
  summarise(avg_delay = mean(delay),
            n = n(),
            .by = month)

usethis::use_data(csection, overwrite = TRUE)
