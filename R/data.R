#' Ceasearian Section Delay
#'
#' Delay times for grade 2 C-sections
#'
#' @format ## `csection`
#' A data frame with 24 rows and 3 columns:
#' \describe{
#'   \item{month}{First day of month.}
#'   \item{avg_delay}{Time in minutes from decision to delivery.}
#'   \item{n}{Number of C-sections.}
#' }
"csection"

#' On-time CT scan
#'
#' Patients with acute abdomen CT scanned within 3 hours after arrival.
#'
#' @format ## `ontime_ct`
#' A data frame with 24 rows and 3 columns:
#' \describe{
#'   \item{month}{First day of month.}
#'   \item{ontime}{Number of patients scanned within 3 hours.}
#'   \item{cases}{Number of patients with acute abdomen.}
#'}
"ontime_ct"

#' Bacteremia Mortality
#'
#' 30-day mortality after bacteremia.
#'
#' @format ## `bacteremia_mortality`
#' A data frame with 143 rows and 4 columns:
#' \describe{
#'   \item{hospital}{Hospital code.}
#'   \item{month}{First day of month}
#'   \item{deaths}{Number of bacteremia patients who died within 30 days from infection.}
#'   \item{cases}{Number of patients with bacteremia.}
#' }
"bacteremia_mortality"

#' Diabetes HbA1c
#'
#' HbA1c measurements in children with diabetes
#'
#' @format
#' A data frame with 43 columns and 3 variables:
#' \describe{
#'   \item{month}{month of measurements.}
#'   \item{avg_hba1c}{average of HbA1c measurements (mmol/mol).}
#'   \item{n}{number of patients who visited the clinic}
#' }
"hba1c"

#' Clostridium difficile infections
#'
#' A dataset with data on hospital acquired Clostridium difficile infections
#' (CDI) before and after an intervention to reduce the risk of CDI.
#'
#' @format A data frame with 36 rows and 5 variables: \itemize{ \item {month}
#'   Month of observation. \item{n} Number of hospital acquired CDI. \item{days}
#'   Number of risk days. A risk day is a patient day without CDI. \item{period}
#'   Factor indicating the period 'pre' or 'post' intervention. \item{notes}
#'   Annotations. }
#' @source www.esundhed.dk (Amager Hvidovre Hospital).
"cdi"
