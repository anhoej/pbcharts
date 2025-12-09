library(data.table)

q2m <- function(y, q) {
  q <- as.integer(substr(q, 1, 1)) * 3 - 2
  q <- as.Date(paste(y, q, '1', sep = '-'))
  q
}

vars <- fread("
              operationsgruppe, operation
              Brok, Hernia
              Galdesten, Gallstones
              Kunstig hofte, Hip replacement
              Kunstig knæ, Knee replacement
              Menisk, Meniscus
              Resten, Other
              ")

orgs <- fread("
              region, reg
              Hele landet, All regions
              Region Hovedstaden, Capital
              Region Midtjylland, Central
              Region Nordjylland, North
              Region Sjælland, East
              Region Syddanmark, South
              ")

waiting_times <- fread('waiting_times.csv') |>
  setnames(tolower)

waiting_times <- waiting_times[
  ,
  let(quarter = q2m(aar, kvartal))
][
  vars,
  on = 'operationsgruppe'
][
  orgs,
  on = 'region'
][
  ,
  .(quarter,
    operation,
    region   = reg,
    avg_days = gns_ventetid,
    n        = antal)
]

waiting_times <- as.data.frame(waiting_times)

usethis::use_data(waiting_times, overwrite = TRUE)
