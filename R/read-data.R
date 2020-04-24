#' Read Morbidities
#'
#' Reads in the list of morbidities used to query Open Targets for associations
#'
#' @param associations dataframe. The target-disease associations
#' @param morbidity_file string. The csv file containing the morbidities list. The
#'    file should consist of two columns: the disease.id (EFO code) and the
#'    human-readable morbidity name. Lines commented out using # are ignored.
#' @return Returns a dataframe with the morbidities and EFO codes
#' @examples
#' read_diseases(associations)
#' read_diseases(associations, filename = "controls_list.csv")
#' @importFrom dplyr mutate mutate_if rename select starts_with contains funs
#' @export
read_diseases = function(filename = "disease_list.csv") {
  d <- read.csv(
    filename,
    col.names = c("disease.id", "morbidity"),
    comment.char = "#",
    stringsAsFactors = FALSE,
    header = FALSE
  )
  return(d)
}

#' Read Associations
#'
#' Reads in the list of target-disease associations retrieved from Open Targets
#'  for the list of morbidities.
#'
#' @param association_file string. The csv file containing the associations
#' @param morbidity_file string. The csv file containing the morbidities and EFO codes
#' @param control logical. Indicates whether to also read in the control associations
#' @param control_association_file string. The csv file containing the associations for control morbidities
#' @param control_morbidity_file string. The csv file containing the control morbidities and EFO codes
#' @param remove_morbidities vector. An optional list of morbidities to remove
#' @param filepath string. Path to files
#' @return Returns the \code{associations} dataframe joined with morbidities
#'    added as an extra columns.
#' @examples
#' read_associations()
#' read_associations(controls = TRUE, controlfilename = "control_associations.csv")
#' read_associations(remove_morbitities = c("premature ageing", "arthritis", NA))
#' @importFrom magrittr '%>%'
#' @importFrom dplyr bind_rows full_join rename_at filter
#' @export
read_associations = function(filepath = NULL,
                             association_file = paste0(filepath, "disease_associations.csv"),
                             morbidity_file = paste0(filepath, "disease_list.csv"),
                             controls = FALSE,
                             control_association_file = paste0(filepath, "control_associations.csv"),
                             control_morbidity_file = paste0(filepath, "control_list.csv"),
                             remove_morbidities = NULL)
  {
  ## Read in the Open Targets associations retrieved via the python client
  associations <-
    read.csv(association_file,
             header = TRUE,
             stringsAsFactors = FALSE)
  if (controls) {
    control_associations <-
      read.csv(control_association_file,
               header = TRUE,
               stringsAsFactors = FALSE)
    associations <-
      bind_rows(associations, control_associations)
  }

  ## Get list of morbidities
  d <- read_diseases(morbidity_file)
  if (controls) {
    d <- bind_rows(read_diseases(control_morbidities))
  }

  ## Bind morbidities together to the associations table
  a <-
    d %>%
    full_join(associations, by = "disease.id") %>%
    filter(!morbidity %in% remove_morbidities)
  return(a)
}

#' Read in Longevity data
#'
#' Read in targets associated with longevity, based on genetic associations
#' from Open Targets
#'
#' @param filepath string. Path to files
#' @param all_datatypes logical. If false, only the overall association score
#'    and the genetic association score datatype columns are returned. If true,
#'    all association scores for all datatypes are returned.
#' @return Returns a dataframe containing target associations with Longevity
#' @examples
#' read_longevity()
#' read_longevity(all_dataypes = TRUE)
#' @importFrom magrittr '%>%'
#' @importFrom dplyr bind_rows full_join rename_at filter
#' @export
read_longevity <- function(filepath = NULL,
                           all_datatypes = FALSE)
{
  l <-
    read.csv(
      paste0(
        filepath,
        "databases/targets_associated_with_longevity.csv"
      ),
      stringsAsFactors = FALSE
    ) %>%
    select(target.id, starts_with("association")) %>%
    rename_at(dplyr::vars(starts_with("association")), dplyr::funs(paste0("longevity.", .)))
  if (!all_datatypes) {
    l <- l %>%
      dplyr::select(
        target.id,
        longevity.association_score.overall,
        longevity.association_score.datatypes.genetic_association
      )
  }
  return(l)
}
