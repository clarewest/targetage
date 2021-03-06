
# targetage

<!-- badges: start -->
<!-- badges: end -->

A package for exploring potential targets for ageing.

## Installation

``` r
install.packages("targetage")
```

## Example


``` r
library(targetage)
library(tidyverse)

dir = "~/work/ageing-target-pipeline/"

## read in the associations file, add data from genage and targets associated with longevity
a <- read_associations(filepath = dir, remove_morbidities = c("premature ageing", NA)) %>%
  add_genage(filepath = dir) 
a <- a %>% filter(association_score.datatypes.genetic_association >= 0.05)
  
a_long <- a %>% add_longevity(from_file = FALSE)
  
## convert the associations to wide format
a_wide <- widen_associations(a) %>%
  add_genage(filepath = dir)
  
## Label whether SGC targets
a_sgc <- add_adpd_targets(a_wide, filepath = dir)

## generate an UpSetR plot
library(UpSetR)
plot_upset(associations_wide = a_wide, only_longevity = TRUE)

sgc_queries = list(
    list(
      query = elements,
      params = list("AD.sgc.target", 1),
      active = T,
      query.name = "SGC AD target"
    )
      )

plot_upset(associations_wide = a_sgc, queries = sgc_queries)

targetofinterest <- "ENSG00000258366"
target_query =  list(
      query = elements,
      params = list("target.id", targetofinterest),
      active = T,
      query.name = targetofinterest
    )


## generate a heatmap plot showing the associations for a given gene
plot_heatmap(a_wide, "APOE")

safety_json <- read_safety(dir)
unsafe_targets <- map(safety_json, names) %>% unlist() %>% names()
a_wide$safety_warning <- as.integer(a_wide$target.gene_info.symbol %in% unsafe_targets)
nrow(a_wide)
sum(a_wide$safety_warning)



```

