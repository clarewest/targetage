#' UpSetR plot
#'
#' Generates an UpSetR plot for the number of targets associated with each
#' combination of morbidities
#'
#' @param associations_wide dataframe. The target-disease associations in wide
#'    format, in which each row is a unique target and the associations with
#'    different morbidities are columns
#' @param associations dataframe. The target-disease associations in long
#'    format, in which each row is a target-disease association
#' @param association_type string. The type of association to include in the
#'    plot, e.g. "genetic" or "overall" associations
#' @param only_longevity logical. When true, only targets associated with
#'    longevity are included in the plot
#' @param morbidities_intersection list. A list of morbidities for which the
#'    intersection should be highlighted in the plot
#' @param morbidities_ignore vector. Morbidities to ignore when plotting
#' @param queries list. Custom queries
#' @return Returns an UpSetR plot showing the number of targets associated with
#'    each combination of morbidities
#' @examples
#' \dontrun{plot_upset(df, morbidities_intersection = list("heart_disease","neurodegenerative_disease", "longevity"))}
#' \dontrun{plot_upset(associations = df, association_type = "overall", only_longevity = TRUE)}
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate mutate_if rename select starts_with contains funs
#' @export
plot_upset <-
  function(associations = NULL,
           associations_wide = NULL,
           association_type = "genetic",
           only_longevity = TRUE,
           morbidities_intersection = list("heart_disease", "neurodegenerative_disease", "longevity"),
           morbidities_ignore = NULL,
           queries = NULL) {

    requireNamespace("UpSetR", quietly = TRUE)

    ## if assocations aren't already wide we need to reshape them
    if (missing(associations_wide)) {
      associations_wide = widen_associations(associations)
    }

    ## Process argument specifying which associations to plot
    type_filter <- switch(association_type,
                          "genetic" = "association_score.datatypes.genetic_association",
                          "overall" = "association_score.overall",
                          stop("Invalid option for association type."))

    binary_associations <- associations_wide %>%
      dplyr::select(target.id, dplyr::contains(type_filter), GenAge.human) %>%
      mutate_if(is.numeric, funs(ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)))) %>%
      mutate(target.id = as.factor(target.id))  %>%
      as.data.frame()

    ## Clean up names for figure
    new_names <-
      colnames(binary_associations) %>% stringr::str_remove(., paste0(".", type_filter))
    colnames(binary_associations) <- new_names

    ## Remove non-diseases and the morbidities to ignore
    diseases <- new_names[! new_names %in% c("target.id", "GenAge.human", "AD.sgc.target", "PD.sgc.target", stringr::str_replace_all(morbidities_ignore, ' ', "_"))]

    ## Only select targets that are associated with longevity
    if (only_longevity){
      binary_associations <- binary_associations %>% dplyr::filter(longevity > 0)
    }

    ## Query to highlight specific intersection if specified
    if (missing(morbidities_intersection)) {
      active_queries = NULL
    } else {
      active_queries = list(
        list(
          query = intersects,
          params = morbidities_intersection,
          color = "orange",
          active = T
        )
      )
    }

    if (!missing(queries)){
      active_queries = queries
    }

    upset_plot <- UpSetR::upset(
      binary_associations,
      sets = diseases,
      sets.bar.color = "#56B4E9",
      order.by = "freq",
      text.scale = c(1.3, 1.3, 1.3, 1, 1.5, 1),
      mainbar.y.label = "",
      sets.x.label = "Total targets",
      queries = active_queries
    )

    return(upset_plot)
  }


