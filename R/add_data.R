add_genage <- function(associations,human = TRUE,mouse = FALSE) {
  ## GenAge human genes
  if (human) {
    genage <-
      read.csv("databases/genage_human.csv", header = TRUE)  %>%
      dplyr::rename(target.gene_info.symbol = symbol,
                    GenAge.human.why = why) %>%
      dplyr::select(GenAge.ID,
                    target.gene_info.symbol,
                    GenAge.human.why,
                    entrez.gene.id) %>%
      mutate(GenAge.human = 1)
    ## join genage with associations
    associations <-
      associations %>% left_join(genage, by = "target.gene_info.symbol")
  }
