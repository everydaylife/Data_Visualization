# Network Visualization
# Jumpstart code from Thomas Miller

# bring in packages we rely upon for work in predictive analytics
library(igraph)  # network/graph methods
library(network)  # network representations
library(intergraph)  # for exchanges between igraph and network

# Data Prep

# ----------------------------------------------------------
# Read in list of links... (from-node, to-node) pairs
# ----------------------------------------------------------
all_enron_links <- read.table('enron_email_links.txt', header = FALSE)
cat("\n\nNumber of Links on Input: ", nrow(all_enron_links))
# check the structure of the input data data frame
print(str(all_enron_links))

# consider non-zero nodes only
non_zero_enron_links <- subset(all_enron_links, subset = (V1 != 0))
non_zero_enron_links <- subset(non_zero_enron_links, subset = (V2 != 0))

# ensure that no e-mail links are from an executive to himself/herself
# i.e. eliminate any nodes that are self-referring 
enron_links <- subset(non_zero_enron_links, subset = (V1 != V2))
cat("\n\nNumber of Valid Links: ", nrow(enron_links))

# create network object from the links
# multiple = TRUE allows for multiplex links/edges
# because it is possible to have two or more links
# between the same two nodes (multiple e-mail messages
# between the same two people)
enron_net <- network(as.matrix(enron_links),
    matrix.type = "edgelist", directed = TRUE, multiple = TRUE)
# create graph object with intergraph function asIgraph()
enron_graph <- asIgraph(enron_net)

# set up node reference table/data frame for later subgraph selection
node_index <- as.numeric(V(enron_graph))
V(enron_graph)$name <- node_name <- as.character(V(enron_graph))
node_name <- as.character(node_index)
node_reference_table <- data.frame(node_index, node_name)

# consider the subgraph of all people that node "1"
# communicates with by e-mail (mail in or out)
ego_1_mail <- induced.subgraph(enron_graph, 
    neighborhood(enron_graph, order = 1, nodes = 1)[[1]])
# examine alternative layouts for plotting the ego_1_mail 
pdf(file = "fig_ego_1_mail_network_four_ways.pdf", width = 5.5, height = 5.5)
par(mfrow = c(1,1))  # four plots on one page
set.seed(9999)  # for reproducible results
plot(ego_1_mail, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25,
    layout = layout.fruchterman.reingold)
title("Fruchterman-Reingold Layout")   
set.seed(9999)  # for reproducible results
plot(ego_1_mail, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, 
    layout = layout.kamada.kawai)  
title("Kamada-Kawai Layout")    
set.seed(9999)  # for reproducible results
plot(ego_1_mail, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, 
    layout = layout.circle)
title("Circle Layout")     
set.seed(9999)  # for reproducible results
plot(ego_1_mail, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25,
    layout = layout.reingold.tilford)    
title("Reingold-Tilford Layout")       
dev.off()

set.seed(9999)  # for reproducible results
pdf(file = "fig_ego_1_mail_network.pdf", width = 8.5, height = 11)
plot(ego_1_mail, vertex.size = 15, vertex.color = "yellow", 
    vertex.label.cex = 0.9, edge.arrow.size = 0.25, 
    edge.color = "black", layout = layout.kamada.kawai)
dev.off()

# examine the degree of each node in the complete Enron e-mail network
# and add this measure (degree centrality) to the node reference table
node_reference_table$node_degree <- degree(enron_graph)
print(str(node_reference_table))

# calculate betweenness centrality and add to node reference table
node_reference_table$betweenness <- betweenness(enron_graph)
print(str(node_reference_table))

# sort the node reference table by degree and identify the indices
# of the most active nodes (those with the most links)
sorted_node_reference_table <- 
    node_reference_table[sort.list(node_reference_table$node_degree, 
        decreasing = TRUE),]
# check on the sort
print(head(sorted_node_reference_table))
print(tail(sorted_node_reference_table))

# sort the node reference table by betweenness centrality and identify the indices
# of the most active nodes (those with the most links)
bw_sorted_node_reference_table <- 
  node_reference_table[sort.list(node_reference_table$betweenness, 
                                 decreasing = TRUE),]
# check on the sort
print(head(bw_sorted_node_reference_table))
print(tail(bw_sorted_node_reference_table))

# sort the node reference table by closeness centrality and identify the indices
# of the most active nodes (those with the most links)
close_sorted_node_reference_table <- 
  node_reference_table[sort.list(node_reference_table$closeness, 
                                 decreasing = TRUE),]
# check on the sort
print(head(close_sorted_node_reference_table))
print(tail(close_sorted_node_reference_table))

# select the top K executives... set K 
K <- 50
N <- 25
O <- 10

# identify a subset of K Enron executives based on e-mail-activity 
top_node_indices <- sorted_node_reference_table$node_index[1:K]
print(top_node_indices)

# try a few different network sizes to see if graphs are easier to interpret
top_25_indices <- sorted_node_reference_table$node_index[1:N]
print(top_25_indices)

top_10_indices <- sorted_node_reference_table$node_index[1:O]
print(top_10_indices)

# construct the subgraph of the top K executives
top_enron_graph <- induced.subgraph(enron_graph, top_node_indices)
# examine alternative layouts for plotting the top_enron_graph 
pdf(file = "fig_top_enron_graph_four_ways.pdf", width = 5.5, height = 5.5)
par(mfrow = c(1,1))  # four plots on one page
set.seed(9999)  # for reproducible results
plot(top_enron_graph, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25,
    layout = layout.fruchterman.reingold)
title("Fruchterman-Reingold Layout")   
set.seed(9999)  # for reproducible results
plot(top_enron_graph, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, 
    layout = layout.kamada.kawai)  
title("Kamada-Kawai Layout")    
set.seed(9999)  # for reproducible results
plot(top_enron_graph, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25, 
    layout = layout.circle)
title("Circle Layout")     
set.seed(9999)  # for reproducible results
plot(top_enron_graph, vertex.size = 10, vertex.color = "yellow", 
    vertex.label = NA, edge.arrow.size = 0.25,
    layout = layout.reingold.tilford)    
title("Reingold-Tilford Layout")       
dev.off()

# construct the subgraph of the top N executives
top_25_enron_graph <- induced.subgraph(enron_graph, top_25_indices)
# examine alternative layouts for plotting the top_25_enron_graph 
pdf(file = "fig_top_25_enron_graph_four_ways.pdf", width = 5.5, height = 5.5)
par(mfrow = c(1,1))  # four plots on one page
set.seed(9999)  # for reproducible results
plot(top_25_enron_graph, vertex.size = 10, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25,
     layout = layout.fruchterman.reingold)
title("Fruchterman-Reingold Layout")   
set.seed(9999)  # for reproducible results
plot(top_25_enron_graph, vertex.size = 10, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25, 
     layout = layout.kamada.kawai)  
title("Kamada-Kawai Layout")    
set.seed(9999)  # for reproducible results
plot(top_25_enron_graph, vertex.size = 10, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25, 
     layout = layout.circle)
title("Circle Layout")     
set.seed(9999)  # for reproducible results
plot(top_25_enron_graph, vertex.size = 10, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25,
     layout = layout.reingold.tilford)    
title("Reingold-Tilford Layout")       
dev.off()

# construct the subgraph of the top O executives
top_10_enron_graph <- induced.subgraph(enron_graph, top_10_indices)
# examine alternative layouts for plotting the top_10_enron_graph 
pdf(file = "fig_top_10_enron_graph_four_ways.pdf", width = 5.5, height = 5.5)
par(mfrow = c(1,1))  # four plots on one page
set.seed(9999)  # for reproducible results
plot(top_10_enron_graph, vertex.size = 10, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25,
     layout = layout.fruchterman.reingold)
title("Fruchterman-Reingold Layout")   
set.seed(9999)  # for reproducible results
plot(top_10_enron_graph, vertex.size = 10, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25, 
     layout = layout.kamada.kawai)  
title("Kamada-Kawai Layout")    
set.seed(9999)  # for reproducible results
plot(top_10_enron_graph, vertex.size = 10, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25, 
     layout = layout.circle)
title("Circle Layout")     
set.seed(9999)  # for reproducible results
plot(top_10_enron_graph, vertex.size = 10, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25,
     layout = layout.reingold.tilford)    
title("Reingold-Tilford Layout")       
dev.off()

# let's use the Kamada-Kawai layout for the labeled plot of K execs
set.seed(9999)  # for reproducible results
pdf(file = "fig_top_enron_graph.pdf", width = 8.5, height = 11)
plot(top_enron_graph, vertex.size = 15, vertex.color = "yellow", 
    vertex.label.cex = 0.9, edge.arrow.size = 0.25, 
    edge.color = "darkgray", layout = layout.kamada.kawai)
dev.off()

# let's use the Kamada-Kawai layout for the labeled plot of N execs
set.seed(9999)  # for reproducible results
pdf(file = "fig_top_25_enron_graph.pdf", width = 8.5, height = 11)
plot(top_25_enron_graph, vertex.size = 15, vertex.color = "yellow", 
     vertex.label.cex = 0.9, edge.arrow.size = 0.25, 
     edge.color = "darkgray", layout = layout.kamada.kawai)
dev.off()

# let's use the Kamada-Kawai layout for the labeled plot of O execs
set.seed(9999)  # for reproducible results
pdf(file = "fig_top_10_enron_graph.pdf", width = 8.5, height = 11)
plot(top_10_enron_graph, vertex.size = 15, vertex.color = "yellow", 
     vertex.label.cex = 0.9, edge.arrow.size = 0.25, 
     edge.color = "darkgray", layout = layout.kamada.kawai)
dev.off()

# construct the subgraph of the top O executives using betweenness centrality as vertex size
top_10_bw_indices <- bw_sorted_node_reference_table$node_index[1:O]
print(top_10_bw_indices)

top_10_bw_enron_graph <- induced.subgraph(enron_graph, top_10_bw_indices)
# examine alternative layouts for plotting the top_10_enron_graph 
pdf(file = "fig_top_10_bw_enron_graph_four_ways.pdf", width = 5.5, height = 5.5)
par(mfrow = c(1,1))  # four plots on one page
set.seed(9999)  # for reproducible results
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25,
     layout = layout.fruchterman.reingold)
title("Fruchterman-Reingold Layout")   
set.seed(9999)  # for reproducible results
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25, 
     layout = layout.kamada.kawai)  
title("Kamada-Kawai Layout")    
set.seed(9999)  # for reproducible results
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25, 
     layout = layout.circle)
title("Circle Layout")     
set.seed(9999)  # for reproducible results
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25,
     layout = layout.reingold.tilford)    
title("Reingold-Tilford Layout")       
dev.off()

# let's use the Kamada-Kawai layout for the labeled plot
set.seed(9999)  # for reproducible results
pdf(file = "fig_top_bw_enron_graph.pdf", width = 8.5, height = 11)
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness, vertex.color = "yellow", 
     vertex.label.cex = 0.9, edge.arrow.size = 0.25, 
     edge.color = "darkgray", layout = layout.kamada.kawai)
dev.off()

# didn't work; message about sin and cos?
# consider that betweenness values are very large and need to scale them down
top_10_bw_enron_graph <- induced.subgraph(enron_graph, top_10_bw_indices)

# examine alternative layouts for plotting the top_10_enron_graph 
pdf(file = "fig_top_10_bw_enron_graph_four_ways.pdf", width = 5.5, height = 5.5)
par(mfrow = c(1,1))  # four plots on one page
set.seed(9999)  # for reproducible results
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness / 1000000, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25,
     layout = layout.fruchterman.reingold)
title("Fruchterman-Reingold Layout")   
set.seed(9999)  # for reproducible results
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness / 1000000, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25, 
     layout = layout.kamada.kawai)  
title("Kamada-Kawai Layout")    
set.seed(9999)  # for reproducible results
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness / 1000000, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25, 
     layout = layout.circle)
title("Circle Layout")     
set.seed(9999)  # for reproducible results
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness / 1000000, vertex.color = "yellow", 
     vertex.label = NA, edge.arrow.size = 0.25,
     layout = layout.reingold.tilford)    
title("Reingold-Tilford Layout")       
dev.off()

# let's use the Kamada-Kawai layout for the labeled plot of top 10 vertices by betweenness
set.seed(9999)  # for reproducible results
pdf(file = "fig_top_bw_enron_graph.pdf", width = 8.5, height = 11)
plot(top_10_bw_enron_graph, vertex.size = bw_sorted_node_reference_table$betweenness / 1000000, vertex.color = "yellow", 
     vertex.label.cex = 0.9, edge.arrow.size = 0.25, 
     edge.color = "darkgray", layout = layout.kamada.kawai)
dev.off()