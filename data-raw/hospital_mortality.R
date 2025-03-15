library(tidyverse)

hospital_mortality <- read_rds('data-raw/indlaeggelser.rds') |>
  filter(var == 'Dødsfald under indlæggelse',
         periode >= '2022-01-01') |>
  mutate(hospital = as.character(factor(sygehus, labels = LETTERS[1:5]))) |>
  select(hospital, month = periode, deaths = taeller, discharges = naevner)

usethis::use_data(hospital_mortality, overwrite = TRUE)
