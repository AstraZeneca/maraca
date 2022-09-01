#' maraca package.
#'
#' @docType package
#' @name maraca
NULL

#' Example HCE scenario A.
#'
#' This is example data frame containing the example for scenario A.
#'
#' @docType data
#' @usage data(hce_scenario_a)
#' @format A data frame with 1000 rows.
#' \describe{
#'   \item{X}{Row number}
#'   \item{SUBJID}{The patient identifier}
#'   \item{GROUP}{Which group the row belongs to}
#'   \item{GROUPN}{Not required for computation.
#'                 The group as an arbitrary numerical value}
#'   \item{AVAL}{The TTE value associated to the patient}
#'   \item{AVAL0}{Not required for computation. TTE value offset by GROUPN}
#'   \item{TRTP}{Treatment group}
#' }
"hce_scenario_a"

#' Example HCE scenario B.
#'
#' This is example data frame containing the example for scenario B.
#'
#' @docType data
#' @usage data(hce_scenario_b)
#' @format A data frame with 1000 rows.
#' \describe{
#'   \item{X}{Row number}
#'   \item{SUBJID}{The patient identifier}
#'   \item{GROUP}{Which group the row belongs to}
#'   \item{GROUPN}{Not required for computation.
#'                 The group as an arbitrary numerical value}
#'   \item{AVAL}{The TTE value associated to the patient}
#'   \item{AVAL0}{Not required for computation. TTE value offset by GROUPN}
#'   \item{TRTP}{Treatment group}
#' }
"hce_scenario_b"

#' Example HCE scenario C.
#'
#' This is example data frame containing the example for scenario C.
#'
#' @docType data
#' @usage data(hce_scenario_c)
#' @format A data frame with 1000 rows.
#' \describe{
#'   \item{X}{Row number}
#'   \item{SUBJID}{The patient identifier}
#'   \item{GROUP}{Which group the row belongs to}
#'   \item{GROUPN}{Not required for computation.
#'                 The group as an arbitrary numerical value}
#'   \item{AVAL}{The TTE value associated to the patient}
#'   \item{AVAL0}{Not required for computation. TTE value offset by GROUPN}
#'   \item{TRTP}{Treatment group}
#' }
"hce_scenario_c"

#' Example HCE scenario D.
#'
#' This is example data frame containing the example for scenario D.
#'
#' @docType data
#' @usage data(hce_scenario_d)
#' @format A data frame with 1000 rows.
#' \describe{
#'   \item{X}{Row number}
#'   \item{SUBJID}{The patient identifier}
#'   \item{GROUP}{Which group the row belongs to}
#'   \item{GROUPN}{Not required for computation.
#'                 The group as an arbitrary numerical value}
#'   \item{AVAL}{The TTE value associated to the patient}
#'   \item{AVAL0}{Not required for computation. TTE value offset by GROUPN}
#'   \item{TRTP}{Treatment group}
#' }
"hce_scenario_d"

#' Example HCE scenario KCCQ3.
#'
#' This is example data frame containing the example for scenario KCCQ3.
#'
#' @docType data
#' @usage data(hce_scenario_kccq3)
#' @format A data frame with 5000 rows.
#' \describe{
#'   \item{SUBJID}{The patient identifier}
#'   \item{GROUP}{Which group the row belongs to}
#'   \item{GROUPN}{Not required for computation.
#'                 The group as an arbitrary numerical value}
#'   \item{AVAL}{The TTE value associated to the patient}
#'   \item{AVAL0}{Not required for computation. TTE value offset by GROUPN}
#'   \item{TRTP}{Treatment group}
#'   \item{HFHT}{Not needed}
#'   \item{SEED}{Not needed}
#' }
"hce_scenario_kccq3"
