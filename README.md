
# targetage

<!-- badges: start -->
<!-- badges: end -->

The goal of targetage is to ...

## Installation

You can install the released version of targetage from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("targetage")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(targetage)
library(tidyverse)

dir = "~/work/ageing-target-pipeline/"

## read in the associations file, add data from genage and targets associated with longevity
a <- read_associations(filepath = dir, remove_morbidities = c("premature ageing", NA)) %>%
  add_genage(filepath = dir) 
  
a_long <- a %>% add_longevity(from_file = FALSE)
  
## convert the associations to wide format
a_wide <- widen_associations(a) %>%
  add_genage(filepath = dir)

## generate an UpSetR plot
plot_upset(a_wide, only_longevity = TRUE)

## generate a heatmap plot showing the associations for a given gene
plot_heatmap(a_wide, "APOE")
  
```

