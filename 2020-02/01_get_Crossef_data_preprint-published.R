##this script uses the Crossref API 
##to get information on preprints linked to published versions (and vv)  
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

#define function to get and parse Crossref metadata for API query result
#use cursor to page through result pages
#use low-level API (cr_works_) which gives json or list output (and includes relation)
#set parse = FALSE to get JSON, parse = TRUE to get list output

#set function to get numer of results to use for cursor
getMax <- function(var){
  cr_result <- cr_works_(filter = c(relation_type = var),
                         limit = 1,
                         parse = TRUE)
  max <- cr_result$message$`total-results`
  
  return(max)
}

#set function to query API with cursor
#use low-level API (cr_works_) which gives json or list output (and includes relation)
#set parse = FALSE to get JSON, parse = TRUE to get list output
getData <- function(var, max){
  #get crossref metadata for each doi in vector dois
  cr_result <- cr_works_(filter = c(relation_type = var),
                         limit = 1000,
                         select = c('DOI', 'publisher', 'type', 'created', 'relation'),
                         cursor = "*",
                         cursor_max = max,
                         .progress = TRUE,
                         parse = TRUE) #change to TRUE for list output
}


extractData <- function(res){
  #only keep 'message$items' element of each list element
  #map_depth is awesome but I still don't get regular way...
  cr_list <- res %>%
    map_depth(1, `[[`, "message") %>%
    map_depth(1, `[[`, "items")
  #create dataframe with list column
  #name list column
  cr_result <- map_dfr(cr_list, tibble) %>%
    rename(list = 1) %>%
  #extract elements of list column
  #map_chr (or equivalent) returns atomic vector
    mutate(
      DOI = map_chr(list, "DOI", .null = NULL),
      publisher = map_chr(list, "publisher", .null = NULL),
      type = map_chr(list, "type", .null = NULL),
      created = map(list, "created", .null = NULL),
      relation = map(list, "relation", .null = NULL)) %>%
    select(-list) %>%
  #extract elements of date column
    mutate(
      created = map_chr(created, "date-time", .null = NULL)) %>%
    mutate(created = as.Date(created),
           created_year = lubridate::year(created)) %>%
  #extract elements of relation column
    mutate(
        relation_type = rep(var),
        relation = map(relation, var)) %>%
    unnest(relation) %>%
    mutate(
      relation_DOI = map_chr(relation, "id"),
      assertion = map_chr(relation, "asserted-by")) %>%
    select(-relation) %>%
    #remove duplicate rows (1 in test set)
    distinct() %>%
    #spread column with assertion to not have DOIs listed twice
    mutate(assertion2 = assertion) %>%
    spread(assertion, assertion2) %>%
    rename(asserted_by_object = object,
           asserted_by_subject = subject)
    
  return(cr_result)

}

#set function to query API for relation_DOIs with cursor
#can use high-level API (cr_works) that gives dataframe result b/c no relation info requested
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
    

#set variable
var <- "is-preprint-of"
var <- "has-preprint"

#test with modest max value ;) 
#max <- 2000

#run scripts
max <- getMax(var)
res <- getData(var, max)
data <- extractData(res)

#store as distinct variable
data_is_preprint <- data
data_has_preprint <- data

#save file with current date
date <- Sys.Date()

filename <- paste0("data/Crossref_",var,"_",date,".csv")
write.csv(data, filename, row.names = FALSE)

#intermediate saving
#saveRDS(res, "res.RDS")

