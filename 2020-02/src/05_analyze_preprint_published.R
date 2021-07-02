install.packages("tidyverse")
library(tidyverse)

#read file, all columns as factors unless otherwise specified
pp1 <- read_csv("results/preprint_published_full.csv",
                col_types = cols(.default = "f", 
                                 DOI = "c",
                                 relation_DOI = "c",
                                 `created.x` = "D",
                                 `created.y` = "D",
                                 diff = "d"))

#initial tallies for reporting
result <- pp1 %>%
  group_by(`relation_type.x`) %>%
  count()

result <- pp1 %>%
  group_by(`relation_type.y`) %>%
  count()

result <- pp1 %>%
  group_by(reciprocal) %>%
  count()

result <- pp1 %>%
  filter(!is.na(diff))


result <- pp1 %>%
  filter(is.na(reciprocal)) %>%
  filter(!is.na(diff)) %>%
  filter(!is.na(`relation_type.y`))

#analyse prefixes of missing preprints
result <- pp1 %>%
  filter(is.na(diff)) %>%
  filter(is.na(`relation_type.x`)) %>%
  group_by(`DOI_prefix`) %>%
  count() %>%
  arrange(desc(n))

#analyse prefixes of missing publications
result <- pp1 %>%
  filter(is.na(diff)) %>%
  filter(is.na(`relation_type.y`)) %>%
  group_by(`relation_DOI_prefix`) %>%
  count() %>%
  arrange(desc(n))


selection <- c("10.17605",
               "10.5281",
               "10.13140")



selection <- c("10.6084")

result <- pp1 %>%
  filter(relation_DOI_prefix %in% selection)



#-------------------------------------------------------------
#set function for basic analysis
getTally <- function(data){
  
  types <- data %>%
    group_by(type) %>%
    count()
  
  years <- data %>%
    group_by(created_year) %>%
    count()
  
  publishers <- data %>%
    group_by(publisher) %>%
    count() %>%
    arrange(desc(n))
 
  result <- list(publishers,
                 years,
                 types) 
}


























selection <- c("Cold Spring Harbor Laboratory",
               "Center for Open Science",
               "MDPI AG",
               "PeerJ",
               "Wiley",
               "Beilstein Institut")

selection <- c("Copernicus GmbH")
selection <- c("JMIR Publications Inc.")

data <- has_preprint
data <- data %>%
  filter(publisher %in% selection)

tally <- getTally(data)

#------------------------------------------------------------------------------------



#add days b/w preprint and publication (based on DOI created date)
data_inner_join <- data_inner_join %>%
  mutate(diff = `created.y` - `created.x`)
  
selection <- c("Cold Spring Harbor Laboratory",
               "Center for Open Science",
               "MDPI AG",
               "JMIR Publications Inc.",
               "PeerJ",
               "Wiley",
               "Beilstein Institut")

selection <- c("Copernicus GmbH")  

data <- data_inner_join %>%
  filter(`publisher.x` %in% selection) %>%
  #arrange(diff)
  group_by(`publisher.y`) %>%
  count() %>%
  arrange(desc(n))

#-----------------------------------------------------

selection <- c("Copernicus GmbH") 
data_Copernicus <- data_inner_join %>%
  filter(`publisher.x` %in% selection) %>%
  filter(diff >= 0) %>%
  filter(diff <= 1000)

qplot(diff, data = data_Copernicus)

selection <- c( "JMIR Publications Inc.") 
data_JMIR <- data_inner_join %>%
  filter(`publisher.x` %in% selection) %>%
  filter(`publisher.y` %in% selection) %>%
  filter(diff >= -300)

qplot(diff, data = data_JMIR)

#----------------------------------------

data <- is_preprint_of %>%
  group_by(publisher) %>%
  count() %>%
  arrange(desc)

#------------------------------------------------------------------------------------
#pivot example
#https://community.rstudio.com/t/pivot-a-two-way-frequency-table-with-proportions/27615/3
df %>% 
  count(Sex, RSVP) %>% 
  group_by(Sex) %>% 
  mutate(prop = prop.table(n)) %>%
  gather(measure, value, n, prop) %>%
  unite(measure, measure, RSVP) %>% 
  spread(measure, value)