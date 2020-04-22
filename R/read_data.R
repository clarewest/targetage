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

read_associations = function() {
  ## Read in the Open Targets associations retrieved via the python client
  associations <-
    read.csv("disease_associations.csv",
             header = TRUE,
             stringsAsFactors = FALSE)
  control_associations <-
    read.csv("control_associations.csv",
             header = TRUE,
             stringsAsFactors = FALSE)

  ## List of morbidities (including controls)
  d <-
    read_diseases() %>%
    bind_rows(read_diseases("control_list.csv"))

  ## Bind morbidities together to the associations table
  a <- d %>% full_join(bind_rows(associations, control_associations), by = "disease.id") %>% filter(! morbidity %in% c("premature ageing", "arthritis", NA))
  return(a)
}

