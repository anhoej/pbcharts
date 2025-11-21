waiting_times <- read.csv('waiting_times.csv',
                          colClasses = c(qrt = 'Date'))

usethis::use_data(waiting_times, overwrite = TRUE)
