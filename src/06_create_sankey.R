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

#read file, all columns as factors unless otherwise specified
pp1 <- read_csv("results/preprint_published_full.csv",
                col_types = cols(.default = "f", 
                                 DOI = "c",
                                 relation_DOI = "c",
                                 `created.x` = "D",
                                 `created.y` = "D",
                                 diff = "d"))

#define function to create connection data frame
createLinks <- function(x){
  
  links <- x %>%
    #select columns
    select(`relation_type.x`,
           `relation_type.y`,
           diff) %>%
    #cretae column with record numbers (sequential)
    #mutate(record = 1:n()) %>%
    #create source and target columns
    mutate(source = case_when(
      !is.na(`relation_type.x`) ~ "is_preprint_of",
      (is.na(`relation_type.x`) & !is.na(`diff`)) ~ "preprint via DOI",
      TRUE ~ "preprint NA")) %>%
    mutate(target = case_when(
      !is.na(`relation_type.y`) ~ "has_preprint",
      (is.na(`relation_type.y`) & !is.na(`diff`)) ~ "pub via DOI",
      TRUE ~ "pub NA")) %>%
    #keep only source and target
    select(source, target) %>%
    #count occurrences
    group_by(source, target) %>%
    count() %>%
    #column name 'value'
    rename(value = n) %>%
    #as dataframe
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
 
#define function to order node dataframe
#listing every entity involved in the flow in order to appear in graph
createNodesOrder <- function(nodes, target){
  nodes <- nodes %>% 
    right_join(data.frame(name = target), by = "name")
  
  return(nodes)
  
}

#define function to create concatenated names (with numbers)
concatenateNames <- function(links, nodes){
  #add numbers, create concatenated names
  sum_source <- links %>%
    group_by(source) %>%
    summarise(sum = sum(value)) %>%
    rename(name = source)
  
  sum_target <- links %>%
    group_by(target) %>%
    summarise(sum = sum(value)) %>%
    rename(name = target)
  
  sum_name <- bind_rows(sum_source,
                        sum_target) %>%
    mutate(name_concat = paste0(name, " (", sum, ")")) %>%
    select(-sum)
  
  #replace names with concatenated names
  nodes <- nodes %>%
    left_join(sum_name, by = "name") %>%
    mutate(name = name_concat) %>%
    select(name)
  
  links <- links %>%
    left_join(sum_name, by = c("source" = "name")) %>%
    left_join(sum_name, by = c("target" = "name")) %>%
    mutate(source = `name_concat.x`,
           target = `name_concat.y`) %>%
    select(source, target, value)
  
  list <- list(links, nodes)

  return(list)
  
}

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
                     nodeWidth=30, fontSize=14, nodePadding=20)
  
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

#create links and nodes dataframe
links <- createLinks(pp1)
nodes <- createNodes(links)

#order nodes 
target <- c("is_preprint_of",
            "preprint via DOI", 
            "preprint NA",
            "pub NA",
            "pub via DOI",
            "has_preprint")

nodes <- createNodesOrder(nodes, target)

#if needed: change names to concatenated names+numbers
#concat <- concatenateNames(links, nodes)
#links <- concat[[1]]
#nodes <- concat[[2]]

#add IDs to links, create plot
links <- modifyLinks(links, nodes)
p <- plotSankey(links, nodes)

#save plot as html + png
plotname <- "sankey1"
file_html <- paste0(plotname, ".html")
file_png <- paste0("img/", plotname, ".png")

#get dimensions of viewer window (width, height)
dim <- grDevices::dev.size("px")

#create html output and from there png output
#create html in wd
saveWidget(p, file_html)
webshot(file_html, file_png, vwidth = dim[1], vheight = dim[2])

#remove html output
unlink(file_html)

