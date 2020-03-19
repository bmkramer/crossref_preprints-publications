#info on Sankey in R: 
#https://www.r-graph-gallery.com/sankey-diagram.html
#https://www.r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html

#install.packages("tidyverse")
#install.packages("networkD3")
#install.packages("htmlwidgets")
#install.packages("webshot")
library(tidyverse)
library(networkD3)
library(htmlwidgets)
library(webshot)
webshot::install_phantomjs()


#define function to create connection data frame
createLinks <- function(x){
  links <- x %>%
    #select columns
    select(`publisher.x`,
           `publisher.y`,
           diff) %>%
    #filter out incomplete matches
    filter(!is.na(diff)) %>%
    select(-diff) %>%
    #create source and target columns
    rename(source = `publisher.x`,
           target = `publisher.y`) %>%
    #count occurrences
    group_by(source, target) %>%
    count() %>%
    #column name 'value'
    rename(value = n) %>%
    #as dataframe
    as.data.frame()
  
  return(links)
}

#define function to create links from modified links dataframe
#with nodes replaced by "other"

createNewLinks <- function(x){
  links <- x %>%
    #count occurrences
    group_by(source, target) %>%
    summarise(value = sum(value)) %>%
    as.data.frame()
  
  return(links)
    
}

#define function to create node dataframe
#listing every entity involved in the flow
createNodes <- function(links){
  nodes <- data.frame(
    name = c(as.character(links$source),
             as.character(links$target))) %>% 
    unique()
  
  return(nodes)
}


#define function to count occurrences for source and target
getNodesFrequency <- function(links){

  source_freq <- links %>%
    group_by(source) %>%
    summarise(sum = sum(value)) %>%
    rename(name = source) %>%
    arrange(desc(sum))
  
  target_freq <- links %>%
    group_by(target) %>%
    summarise(sum = sum(value)) %>%
    rename(name = target) %>%
    arrange(desc(sum))
  
  list_freq <- list(source_freq,
                    target_freq)
  
  return(list_freq)
}

#define function to select nodes based on frequencies
selectNodes <- function(list_freq, list_freq_values){
 
  #create 'other' category based on cutoff level
  #need to convert name to character vector
  source_sel <- list_freq[[1]] %>%
    mutate(name = as.character(name)) %>%
    mutate(source2 = case_when(
      sum > list_freq_values[[1]] ~ name,
      sum <= list_freq_values[[1]] ~ "other"))
    
  target_sel <- list_freq[[2]] %>%
      mutate(name = as.character(name)) %>%
      mutate(target2 = case_when(
        sum > list_freq_values[[2]] ~ name,
        sum <= list_freq_values[[2]] ~ "other"))  
    
  list_sel <- list(source_sel,
                   target_sel)
  
  return(list_sel)
  
}    

#define function to rename nodes (short names) + concatenate counts
renameNodes <- function(list_sel_freq, list_names){
  
  #need to convert name to character vector
  source_sel2 <- list_sel_freq[[1]] %>%
    mutate(name = as.character(name)) %>%
    mutate(source1 = list_names[[1]]) %>%
    mutate(source2 = paste0(source1, " (", sum, ")"))
  
  target_sel2 <- list_sel_freq[[2]] %>%
    mutate(name = as.character(name)) %>%
    mutate(target1 = list_names[[2]]) %>%
    mutate(target2 = paste0(target1, " (", sum, ")"))
  
  list_sel2 <- list(source_sel2,
                    target_sel2)
  
  return(list_sel2)
  
}



#define function to implement selection/renaming in links 
replaceNodes <- function(links, list_sel){ 

  links_sel <- links %>%
    #convert source and target into character vectors
    mutate(source = as.character(source),
           target = as.character(target)) %>%
    #replace names by 'other' for source
    left_join(list_sel[[1]], by = c("source" = "name")) %>%
    mutate(source = source2) %>%
    #replace names by 'other' for target
    left_join(list_sel[[2]], by = c("target" = "name")) %>%
    mutate(target = target2) %>%
    select(source, target, value) %>%
    #convert source, target for factors
    mutate(source = as.factor(source),
           target = as.factor(target))
    
  return(links_sel)

}
  


#rename nodes (e.g. abbreviated publisher names)
#NB for simultaneous reorder would do rightjoin iso leftjoin
#renameNodes <- function(nodes, target){
#  nodes <- nodes_sel %>% 
#    mutate(name = as.character(name)) %>%
#    left_join(data.frame(name = target, stringsAsFactors = FALSE), 
#              by = "name")
#  
#  return(nodes)
#}

   



#define function to add ID columns
modifyLinks <- function(links, nodes){
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  return(links)
}
  
#define function to render plot
plotSankey <- function(links, nodes){
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE, 
                     nodeWidth=30, fontSize=14, nodePadding=10)
  
  return(p)
}

#------------------------------------------------------------
#read file, all columns as factors unless otherwise specified
pp1 <- read_csv("results/preprint_published_full.csv",
                col_types = cols(.default = "f", 
                                 DOI = "c",
                                 relation_DOI = "c",
                                 `created.x` = "D",
                                 `created.y` = "D",
                                 diff = "d"))

#create links dataframe
links <- createLinks(pp1)
#get nodes frequency
list_freq <- getNodesFrequency(links)

#inspect frequencies for source & target
#source_freq <- list_freq[[1]]
#target_freq <- list_freq[[2]]

#set cut-off value for frequencies
source_freq_value <- 50
target_freq_value <- 1000

list_freq_values <- list(source_freq_value,
                         target_freq_value)

#select nodes based on frequency cut-off
list_sel <- selectNodes(list_freq, list_freq_values)

#inspect selection for source & target
#source_sel <- list_sel[[1]]
#target_sel <- list_sel[[2]]

#replace node names ('other' below cut-off)
links_sel <- replaceNodes(links, list_sel)

#--------------------------------------------------
#include intermediate steps to relabel nodes
#abbreviate overly long names
#label source and target separately by adding n

#inspect frequencies for source & target
#source_sel_freq <- list_sel_freq[[1]]
#target_sel_freq <- list_sel_freq[[2]]

#define new node names (abbreviated, specific for source and target)
source_names <- c("Cold Spring Harbor Lab.",
                  "Copernicus",
                  "MDPI",
                  "OSF",
                  "JMIR",
                  "PeerJ",
                  "Fed. Reserve Bank Minn.",
                  "Wiley",
                  "Beilstein Institut",
                  "other")

target_names <- c("Copernicus",
                  "other",
                  "SpringerNature",
                  "MDPI",
                  "Elsevier",
                  "JMIR",
                  "PLOS",
                  "OUP",
                  "Wiley",
                  "PeerJ",
                  "eLife",
                  "Frontiers")

list_names <- list(source_names,
                   target_names)

#create new names for source and target
list_sel2 <- renameNodes(list_sel_freq, list_names)


#inspect new names for source & target
#source_sel2 <- list_sel2[[1]]
#target_sel2 <- list_sel2[[2]]

#replace names in links
links_sel2 <- replaceNodes(links_sel, list_sel2)

#---------------------------------------------------
#recreate links dataframe
links_sel3 <- createNewLinks(links_sel2)

#create nodes list
nodes <- createNodes(links_sel3)

#add IDs to links
links_sel3 <- modifyLinks(links_sel3, nodes)
#plot
p <- plotSankey(links_sel3, nodes)

p

#save plot as html + png
plotname <- "sankey_publishers_2"
file_html <- paste0(plotname, ".html")
file_png <- paste0("img/", plotname, ".png")

#get dimensions of viewer window (width, height)
dim <- grDevices::dev.size("px")

#create html output and from there png output
#create html in wd
saveWidget(p, file_html)
#webshot(file_html, file_png, vwidth = dim[1], vheight = dim[2])
#or unsized
webshot(file_html, file_png)


#remove html output
unlink(file_html)

