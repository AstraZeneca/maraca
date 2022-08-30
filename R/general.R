#' maraca package
#' @docType package
#' @name maraca
NULL

#' Example HCE scenario A
#'
#' This is example data frame containing the example for scenario A
#'
#' @docType data
#' @usage data(hce_scenario_a)
#' @format A data frame with 20 variables and 124 rows.
#' \describe{
#'   \item{X}{Row number}
#'   \item{LDHHIFL}{}
#'   \item{pid}{Patient identifier}
#'   \item{ev_pfs_irc_2007}{PFS event indicator (IRC)}
#'   \item{t2_os_mo}{Time to death (months)}
#'   \item{ev_os}{Death indicator}
#'   \item{blk_l_5}{}
#'   \item{age}{Patient age}
#'   \item{smipi_low}{}
#'   \item{smipi_intermediate}{}
#'   \item{ecog_2}{ECOG score of 2 or more}
#'   \item{ecog_0_to_1}{ECOG score of less than 2}
#'   \item{bone_marrow}{}
#'   \item{extranodal_disease}{}
#'   \item{gr34_anemia}{}
#'   \item{gr34_neutropenia}{}
#'   \item{gr34_thrombocytopenia}{}
#'   \item{Continuous1}{Continuous covariate 1}
#'   \item{Continuous2}{Continuous covariate 2}
#'   \item{t2_pfs_mo_irc_2007}{Time to the first of progresion of death
#'   (months; IRC)}
#' }
"our_data"
