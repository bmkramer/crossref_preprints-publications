#identify cases where preprint and published article are not reciprocal
#these will not have info for DOIs fields filled (publisher, type, created)
#need to query Crossref separately to get that info

#install.packages("tidyverse")
library(tidyverse)

#join tables together
#read files
is_preprint_of <- read_csv("data/Crossref_is-preprint-of_2020-02-25.csv")
has_preprint <- read_csv("data/Crossref_has-preprint_2020-02-25.csv")

#convert relation_DOIs to lowercase (for good matching in join)
is_preprint_of <- is_preprint_of %>%
  mutate(relation_DOI = tolower(relation_DOI))
  
has_preprint <- has_preprint %>%
  mutate(relation_DOI = tolower(relation_DOI))

#join initial dataframes - combining info on preprints and published articles
data_full_join <- full_join(is_preprint_of,
                              has_preprint,
                              by = c("DOI" = "relation_DOI", "relation_DOI" = "DOI"))


#add column to mark reciprocity
data_full_join <- data_full_join %>%
  mutate(reciprocal = case_when(
    (!is.na(`relation_type.x`) & !is.na(`relation_type.y`)) ~ "reciprocal",
    TRUE ~ NA_character_))

data_missing <- data_full_join %>%
  filter(is.na(reciprocal))

write.csv(data_missing, "results/data_missing.csv", row.names = FALSE)
