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

#' Hospital Mortality
#'
#' Patients who die during hospitalisation
#'
#' @format ## `hospital_mortality`
#' A data frame with 135 rows and 4 columns:
#' \describe{
#'   \item{hospital}{Hospital.}
#'   \item{month}{First day of month.}
#'   \item{deaths}{Number of patients who died during hospitalisation.}
#'   \item{discharges}{Number of discharged patients.}
#' }
"hospital_mortality"
