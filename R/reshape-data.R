#' Widen Associations
#'
#' Reshapes association data from long format (in which each row is a target-disease
#' association) to wide format (in which each row is a unique target and the associations with
#' different morbidities are columns)
#'
#' @param associations dataframe. The target-disease associations in long
#'    format, in which each row is a target-disease association
#' @return Returns a dataframe of the target-disease associations in wide
#'    format, in which each row is a unique target and the associations with
#'    different morbidities are columns
#' @examples
#' associations_wide <- widen_associations(associations)
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate recode select starts_with
#' @export
widen_associations <- function(associations) {
  associations <- unique(associations)

  ## remove spaces from morbidities, and abbreviate appropriate ones
  associations_wide <-
    associations %>%
    mutate(
      morbidity =
        recode(
          morbidity,
          `idiopathic pulmonary fibrosis` = "IPF",
          `chronic obstructive pulmonary disease` = "COPD"
        ),
      is_direct = recode(`is_direct`, `True` = 1, `False` = 0)
    ) %>%
    mutate(morbidity = stringr::str_replace_all(morbidity, " ", "_")) %>%
    dplyr::select(
      starts_with("target"),
      morbidity,
      contains(".datatypes."),
      contains(".overall"),
      is_direct,
      evidence_count.total,
      -id
    ) %>%
    ## pivot longer so each row is a target-disease-associationtype x value
    tidyr::pivot_longer(
      cols = c(
        starts_with("evidence_count.datatypes"),
        evidence_count.total,
        association_score.overall,
        starts_with("association_score.datatypes"),
        is_direct
      )
    ) %>%
    # pivot wider so each row is a target x diseaseassociations
    tidyr::pivot_wider(
      names_from = c("morbidity", "name"),
      values_from = value,
      names_sep = "."
    )
  return(associations_wide)
}
