avoidable_hospitalisations <- read.csv('avoidable_hospitalisations.csv',
                                       col.names = c('municipality',
                                                     'diagnosis',
                                                     'month',
                                                     'admissions',
                                                     'population'),
                                       colClasses = c(municipality = 'character',
                                                      diagnosis = 'character',
                                                      month = 'Date',
                                                      admissions = 'integer',
                                                      population = 'integer'))

avoidable_hospitalisations <- subset(avoidable_hospitalisations,
                                     municipality != 'All municipalities' &
                                       diagnosis != 'All diagnoses')

usethis::use_data(avoidable_hospitalisations, overwrite = TRUE)
