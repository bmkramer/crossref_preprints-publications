#install.packages("tidyverse")
#install.packages("lubridate)
#install.packages("rcrossref")
library(tidyverse)
library(lubridate)
library(rcrossref)

#set email in Renviron
file.edit("~/.Renviron")
#add email address to be shared with Crossref:
#crossref_email = name@example.com
#save the file and restart your R session
#how to remove: delete email from ~/.Renviron

#set function to query API for relation_DOIs with cursor
#can use high-level API (cr_works) that gives dataframe result (b/c no relation info requested)
getData2 <- function(dois){
  #get crossref metadata for each doi in vector dois
  cr_result <- cr_works(dois = dois,
                        #select = c('DOI', 'publisher', 'type', 'created'),
                        .progress = "time")
  
  cr_result <- cr_result$data %>%
    select(doi, publisher, type, created)
  
  return(cr_result)
  
}

#set function to extract data from getData2 result
#for relation_DOIs
extractData2 <- function(res){
  #extract relevant list element from res - this is already a dataframe
  cr_result <- res %>%
    rename(DOI = doi) %>%
    mutate(created = as.Date(created),
           created_year = lubridate::year(created))
  
  return(cr_result)
}



pp1 <- read_csv("data/preprint_published_full.csv")

pubs <- pp1 %>%
  filter(is.na(`publisher.y`)) %>%
  select(`relation_DOI`) %>%
  arrange(`relation_DOI`) %>%
  separate(relation_DOI, 
           into = "prefix", "/", 
           remove = FALSE,
           extra = "drop") %>%
  mutate(prefix = as.factor(prefix))

pubs_prefix_freq <- pubs %>%
  group_by(prefix) %>%
  count() %>%
  arrange(desc(n))

#selection non-Crossref prefixes
#OSF, Zenodo, ResearchGate
selection <- c("10.17605",
               "10.5281",
               "10.13140")

pubs_sel <- pubs %>%
  filter(!prefix %in% selection)

dois <- pull(pubs_sel, relation_DOI)

res <- getData2(dois)
data <- extractData2(res)

missing_pubs <- read_csv("data/missing_pubs_2020-02-29.csv")

missing_pubs <- bind_rows(missing_pubs,
                          data)

missing_pubs <- missing_pubs %>%
  distinct()

write.csv(missing_pubs,"data/missing_pubs_2020-02-29.csv", row.names = FALSE)


