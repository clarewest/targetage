#' Get multimorbidity count
#'
#' Score targets based on the number of morbidities with a genetic association
#'
#' @param associations dataframe. The target-disease associations in wide format
#' @param minscore float. The minimum genetic association score to consider to be an association
#' @param multimorbidityscore boolean. Whether to also calculate the multimorbidity score
#' @param multimorbiditymean boolean. Whether to also calculate the mean of the association scores
#' @return Returns the \code{associations} dataframe with the multimorbidity count as an extra column
#' @examples
#' \dontrun{multimorbidity_score(associations_wide)}
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate mutate_if rename select left_join
#' @export
multimorbidity_count <-
  function(associations_wide, minscore = 0.05, multimorbidityscore = FALSE, multimorbiditymean = FALSE) {
    associations_morbidity <-
      associations_wide %>%
      mutate(multimorbidity.count =
               rowSums(select(., contains("association_score.datatypes.genetic_association")) > minscore, na.rm = TRUE))

    if (multimorbidityscore) {
      associations_morbidity <- associations_morbidity %>% multimorbidity_score()
    }

    if (multimorbiditymean) {
      associations_morbidity <- associations_morbidity %>% multimorbidity_mean()
    }

    return(associations_morbidity)
  }

#' Get multimorbidity score
#'
#' Score targets based on the scores of the multimorbidity associations
#'
#' @param associations dataframe. The target-disease associations in wide format
#' @param minscore float. The minimum genetic association score to consider to be an association
#' @return Returns the \code{associations} dataframe with the multimorbidity score as an extra column
multimorbidity_score <-
  function(associations_wide){
    ## multimorbidity score is the sum of the genetic association scores above 0.05
    associations_morbidity <-
      associations_wide %>%
      mutate(multimorbidity.score =
               rowSums(
                 select(., contains("association_score.datatypes.genetic_association")),
                 na.rm = TRUE))
    return(associations_morbidity)
  }

#' Get multimorbidity mean score
#'
#' Score targets based on the scores of the multimorbidity associations
#'
#' @param associations dataframe. The target-disease associations in wide format
#' @param minscore float. The minimum genetic association score to consider to be an association
#' @return Returns the \code{associations} dataframe with the mean association score as an extra column
multimorbidity_mean <-
  function(associations_wide){
   associations_multimorbidity <-
     associations_wide %>%
     mutate(multimorbidity.mean =
              rowMeans(select(., contains("association_score.datatypes.genetic_association")), na.rm = TRUE))
  }


