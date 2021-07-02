##this script uses the Crossref API 
##to retrieve information on preprints and published versions based on DOI 
#Crossref REST API information: https://github.com/CrossRef/rest-api-doc
#rcrossref manual: https://cran.r-project.org/web/packages/rcrossref/rcrossref.pdf

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
getData2 <- function(dois, i){
  #get crossref metadata for each doi in vector dois
  h <- i-99
  dois <- dois[h:i]
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
    
#read df of missing data
#NB read_csv gives warnings on column classes
data_missing <- read.csv("data/data_missing.csv", stringsAsFactors = FALSE)

#create DOI vectors
dois_preprint <- data_missing %>%
  filter(is.na(`relation_type.x`)) %>%
  pull(DOI) %>%
  unique()

dois_pub <- data_missing %>%
  filter(is.na(`relation_type.y`)) %>%
  pull(relation_DOI) %>%
  unique()

  
#set dois for querying Crossref
dois <- dois_preprint
dois <- dois_pub

c <- seq(100, length(dois), by=100)

#once again, for loop instead of map..
res <- list()
for (i in c){
  result <- getData2(dois, i)
  j <- i/100
  res[[j]] <- result
}

#intermediate saving
#saveRDS(res, "res.RDS")

data <- bind_rows(res) %>%
  extractData2()


#rename file (as appropriate)
missing_preprints <- data
missing_pubs <-data

#write to csv
date <- Sys.Date()

filename <- paste0("missing_preprints_",date,".csv")
write.csv(missing_preprints, filename, row.names = FALSE)

filename <- paste0("missing_pubs_",date,".csv")
write.csv(missing_pubs, filename, row.names = FALSE)

