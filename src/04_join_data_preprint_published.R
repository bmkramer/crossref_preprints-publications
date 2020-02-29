install.packages("tidyverse")
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

data_full_join <- full_join(is_preprint_of,
                              has_preprint,
                              by = c("DOI" = "relation_DOI", "relation_DOI" = "DOI"))

#add column to mark reciprocity
data_full_join <- data_full_join %>%
  mutate(reciprocal = case_when(
    (!is.na(`relation_type.x`) & !is.na(`relation_type.y`)) ~ "reciprocal",
    TRUE ~ NA_character_))


#add retrieved missing data 
#read files
missing_preprints <- read_csv("data/missing_preprints_2020-02-29.csv")
missing_pubs <- read_csv("data/missing_pubs_2020-02-29.csv")

#deduplicate
missing_preprints <- distinct(missing_preprints)
missing_pubs <- distinct(missing_pubs)

#from full_join, filter column with incomplete preprint/pub information
data_full_join_preprint_missing <- data_full_join %>%
  filter(is.na(`publisher.x`))

data_full_join_pub_missing <- data_full_join %>%
  filter(is.na(`publisher.y`))

data_full_join_reciprocal <- data_full_join %>%
  filter(!is.na(reciprocal))

#approach for joining: left join missing data
#then remove/rearrange columns
data_full_join_preprint_added <- left_join(data_full_join_preprint_missing,
                                            missing_preprints,
                                            by = c("DOI" = "DOI"))

data_full_join_preprint_added <- data_full_join_preprint_added %>%
  mutate(`publisher.x` = publisher,
         `type.x` = type,
         `created.x` = created,
         `created_year.x` = created_year) %>%
  select(-c(publisher,
            type,
            created,
            created_year))

data_full_join_pub_added <- left_join(data_full_join_pub_missing,
                                           missing_pubs,
                                           by = c("relation_DOI" = "DOI"))

data_full_join_pub_added <- data_full_join_pub_added %>%
  mutate(`publisher.y` = publisher,
         `type.y` = type,
         `created.y` = created,
         `created_year.y` = created_year) %>%
  select(-c(publisher,
            type,
            created,
            created_year))

#bind 3 sets (reciprocal, missing preprints, missing pubs)
preprint_published_full <- bind_rows(data_full_join_reciprocal,
                                     data_full_join_preprint_added,
                                     data_full_join_pub_added)

#---------------------------------------------------------------
#check remaining missing DOIs
missing_preprints_left <- preprint_published_full %>%
  filter(is.na(`publisher.x`))

missing_pubs_left <- preprint_published_full %>%
  filter(is.na(`publisher.y`))
#---------------------------------------------------------------

#make sorting/selecting parameters into factors
preprint_published_full <- preprint_published_full %>%
  mutate(
    `publisher.x` = as_factor(`publisher.x`),
    `publisher.y` = as_factor(`publisher.y`),
    `type.x` = as_factor(`type.x`),
    `type.y` = as_factor(`type.y`),
    `created_year.x` = as_factor(`created_year.x`),
    `created_year.y` = as_factor(`created_year.y`),
    `relation_type.x` = as_factor(`relation_type.x`),
    `relation_type.y` = as_factor(`relation_type.y`),
    `asserted_by_object.x` = as_factor(`asserted_by_object.x`),
    `asserted_by_object.y`= as_factor(`asserted_by_object.y`),
    `asserted_by_subject.x` = as_factor(`asserted_by_subject.x`),
    `asserted_by_subject.y` = as_factor(`asserted_by_subject.y`),
    reciprocal = as_factor(reciprocal))

#add days b/w preprint and publication (based on DOI created date)
preprint_published_full <- preprint_published_full %>%
  mutate(diff = `created.y` - `created.x`)



write.csv(preprint_published_full, 
          "results/preprint_published_full.csv", 
          row.names = FALSE)
