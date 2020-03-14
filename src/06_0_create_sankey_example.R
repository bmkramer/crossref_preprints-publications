#info on Sankey in R: 
#https://www.r-graph-gallery.com/sankey-diagram.html
#https://www.r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html

#install.packages("tidyverse")
install.packages("networkD3")
library(tidyverse)
library(networkD3)

#FROM CONNNECTION FRAME

# A connection data frame is a list of flows with intensity for each flow
links1 <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes1 <- data.frame(
  name=c(as.character(links1$source), 
         as.character(links1$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links1$IDsource <- match(links1$source, nodes1$name)-1 
links1$IDtarget <- match(links1$target, nodes1$name)-1

# Make the Network
p <- sankeyNetwork(Links = links1, Nodes = nodes1,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

#FROM INCIDENCE MATRIX
# Create an incidence matrix. Usually the flow goes from the row names to the column names.
# Remember that our connection are directed since we are working with a flow.
set.seed(1)
data <- matrix(sample( seq(0,40), 49, replace=T ), 7, 7)
data[data < 35] <- 0
colnames(data) = rownames(data) = c("group_A", "group_B", "group_C", "group_D", "group_E", "group_F", "group_G")

# Transform it to connection data frame with tidyr from the tidyverse:
links2 <- data %>% 
  as.data.frame() %>% 
  rownames_to_column(var="source") %>% 
  gather(key="target", value="value", -1) %>%
  filter(value != 0)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes2 <- data.frame(
  name=c(as.character(links2$source), as.character(links2$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links2$IDsource <- match(links2$source, nodes2$name)-1 
links2$IDtarget <- match(links2$target, nodes2$name)-1

# Make the Network
p <- sankeyNetwork(Links = links2, Nodes = nodes2,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)

p
