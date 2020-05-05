#' Add GenAge data
#'
#' Add the data from GenAge, a curated database of human ageing-related genes (\code{human}) and a database of ageing- and longevity- associated genes in mice (\code{mouse})
#'
#' @param associations dataframe. The target-disease associations
#' @param human logical. Import human data from GenAge, a curated database that includes the few genes directly related to ageing in humans (n=3) plus the best candidate genes from model organisms
#' @param mouse logical. Import mouse data from GenAge, genes related to longevity and/or ageing in mice, including the max and median lifespan change
#' @param filepath string. Path to files
#' @return Returns the \code{associations} dataframe joined with GenAge data as extra columns.
#' @examples
#' add_genage(associations)
#' add_genage(associations, human = TRUE, mouse = FALSE)
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate mutate_if rename select left_join
#' @export
add_genage <-
  function(associations,
           human = TRUE,
           mouse = FALSE,
           filepath = NULL) {
    ## GenAge human genes
    if (human) {
      genage <-
        read.csv(
          paste0(filepath, "databases/genage_human.csv"),
          header = TRUE,
          stringsAsFactors = FALSE
        )  %>%
        rename(target.gene_info.symbol = symbol,
                      GenAge.human.why = why) %>%
        select(GenAge.ID,
                      target.gene_info.symbol,
                      GenAge.human.why,
                      entrez.gene.id) %>%
        mutate(GenAge.human = 1)
      ## join genage with associations
      associations <-
        associations %>% left_join(genage, by = "target.gene_info.symbol")
    }
    if (mouse) {
      ## get human homologues of mouse genes
      mousehoms <-
        read.csv(
          paste0(filepath, "databases/genage_models_orthologs_mouse.csv"),
          header = TRUE,
          stringsAsFactors = FALSE
        ) %>%
        rename(target.gene_info.symbol = Symbol,
                      GenAge.mouse.gene.symbol = Model.Organism.Symbol) %>%
        select(target.gene_info.symbol, GenAge.mouse.gene.symbol)

      ## mouse GenAge data
      mouseage <-
        read.csv(
          paste0(filepath, "databases/genage_mouse.csv"),
          header = TRUE,
          stringsAsFactors = FALSE
        ) %>%
        rename(
          GenAge.mouse.ID = GenAge.ID,
          GenAge.mouse.gene.symbol = symbol,
          GenAge.mouse.lifespan.effect = lifespan.effect,
          GenAge.mouse.longevity.influence = longevity.influence,
          GenAge.mouse.avg.lifespan.change..max.obsv. = avg.lifespan.change..max.obsv.
        ) %>%
        select(-name, -organism) %>%
        left_join(mousehoms, by = "GenAge.mouse.gene.symbol") %>%
        mutate(GenAge.mouse = 1)
      ## join mouseage with associations
      associations <-
        associations %>% left_join(mouseage, by = "target.gene_info.symbol")
    }
    return(associations)
  }

#' Add Longevity data
#'
#' Add data for targets associated with longevity, based on genetic association from Open Targets
#'
#' @param associations dataframe. The target-disease associations
#' @param filepath string. Path to files
#' @param all_datatypes logical. If false, only the overall association score
#'    and the genetic association score datatype columns are returned. If true,
#'    all association scores for all datatypes are returned.
#' @param from_file logical. If true, read from a file, otherwise take from
#'    within the associations dataframe
#' @return Returns the \code{associations} dataframe joined with Longevity data as extra columns.
#' @examples
#' add_longevity(associations)
#' add_longevity(associations, all_datatypes = TRUE)
#' @export
add_longevity <-
  function(associations,
           filepath = NULL,
           all_datatypes = FALSE,
           from_file = FALSE)
  {
    a <- associations %>%
      dplyr::left_join(
        read_longevity(associations,
          filepath = filepath,
          all_datatypes = all_datatypes,
          from_file = from_file
        ),
        fill = TRUE,
        by = "target.id"
      )
    return(a)
  }

