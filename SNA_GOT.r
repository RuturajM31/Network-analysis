
#--------------------### GOT - SOCIAL NETWORK ANALYSIS ### --------------------------------------#

library(tidyverse)
library(igraph)
library(statnet)
library(NetIndices)

setwd("C:/Socialnetwork")

load("union_edges.RData")
load("union_characters.RData")

str(union_edges)
str(union_characters)
head(union_edges)
head(union_characters)

#### load the data into a network & Plot ####

union_graph <- graph_from_data_frame(union_edges, directed = TRUE, vertices = union_characters)
union_graph

#For plotting the legend, I am summarizing the edge and node colors.

color_vertices <- union_characters %>%
  group_by(house, color) %>%
  summarise(n = n()) %>%
  filter(!is.na(color))

colors_edges <- union_edges %>%
  group_by(type, color) %>%
  summarise(n = n()) %>%
  filter(!is.na(color))

layout <- layout_with_fr(union_graph)

plot(union_graph,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph)$name),
     vertex.shape = V(union_graph)$shape,
     vertex.color = V(union_graph)$color, 
     vertex.size = (V(union_graph)$popularity + 0.5) * 5, 
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     edge.color = E(union_graph)$color,
     edge.lty = E(union_graph)$lty)
legend("topleft", legend = c(NA, "Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 4, cex = 2, bty = "n", ncol = 1,
       title = "") 
legend("topleft", legend = "", cex = 3, bty = "n", ncol = 1,
       title = "GOT - Family Ties")

#Node color shows the major houses, node size the character's popularity and node shape their gender (square for male, circle for female). 
#Edge color shows interaction type.

#We consider a character "important" if he has connections to many other characters.
#There are a few network properties, that tell us more about this. 
#For this, I am considering the network as undirected to account for parent/child relationships as being mutual.

union_graph_undir <- as.undirected(union_graph, mode = "collapse")

#### Centrality Entire Network ####

#Centrality describes the number of edges that are in- or outgoing to/from nodes.
#High centrality networks have few nodes with many connections, low centrality networks have many nodes with similar numbers of edges.


#Entire Network
centr_degree(union_graph_undir, mode = "total")$centralization

centr_clo(union_graph_undir, mode = "total")$centralization

centr_eigen(union_graph_undir, directed = FALSE)$centralization


#### Node degree ####

#Node degree or degree centrality describes how central a node
#is in the network (i.e. how many in- and outgoing edges it has or to how many other nodes it is directly connected via one edge).

union_graph_undir_degree <- igraph::degree(union_graph_undir, mode = "total")

#standardized by number of nodes
union_graph_undir_degree_std <- union_graph_undir_degree / (vcount(union_graph_undir) - 1)

node_degree <- data.frame(degree = union_graph_undir_degree,
                          degree_std = union_graph_undir_degree_std) %>%
  tibble::rownames_to_column()

union_characters <- left_join(union_characters, node_degree, by = c("name" = "rowname"))

node_degree %>%
  arrange(-degree) %>%
  .[1:10, ]

#### Closeness ####
##The closeness of a node describes its distance to all other nodes.
#A node with highest closeness is more central and can spread information to many other nodes.

closeness <- igraph::closeness(union_graph_undir, mode = "total")

#standardized by number of nodes
closeness_std <- closeness / (vcount(union_graph_undir) - 1)
node_closeness <- data.frame(closeness = closeness,
                             closeness_std = closeness_std) %>%
  tibble::rownames_to_column()

union_characters <- left_join(union_characters, node_closeness, by = c("name" = "rowname"))

node_closeness %>%
  arrange(-closeness) %>%
  .[1:10, ]

#### Betweenness centrality ####
#Betweenness describes the number of shortest paths between nodes. 
#Nodes with high betweenness centrality are on the path between many other nodes, i.e. they are people who are key connections or bridges between different groups of nodes.
#In a social network, these nodes would be very important because they are likely to pass on information to a wide reach of people.

##Node (Vertex) Betweeness 
betweenness <- igraph::betweenness(union_graph_undir, directed = FALSE)

# standardize by number of node pairs
betweenness_std <- betweenness / ((vcount(union_graph_undir) - 1) * (vcount(union_graph_undir) - 2) / 2)

node_betweenness <- data.frame(betweenness = betweenness,
                               betweenness_std = betweenness_std) %>%
  tibble::rownames_to_column() 

union_characters <- left_join(union_characters, node_betweenness, by = c("name" = "rowname"))

node_betweenness %>%
  arrange(-betweenness) %>%
  .[1:10, ]

## Edge Betweeness 
edge_betweenness <- igraph::edge_betweenness(union_graph_undir, directed = FALSE)

data.frame(edge = attr(E(union_graph_undir), "vnames"),
           betweenness = edge_betweenness) %>%
  tibble::rownames_to_column() %>%
  arrange(-betweenness) %>%
  .[1:10, ]


#PLOT
#This, we can now plot by feeding the node betweenness as vertex.size and edge betweenness as edge.width to our plot function:
  
plot(union_graph_undir,
       layout = layout,
       vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
       vertex.shape = V(union_graph_undir)$shape,
       vertex.color = V(union_graph_undir)$color, 
       vertex.size = betweenness * 0.001, 
       vertex.frame.color = "gray", 
       vertex.label.color = "black", 
       vertex.label.cex = 0.8,
       edge.width = edge_betweenness * 0.01,
       edge.arrow.size = 0.5,
       edge.color = E(union_graph_undir)$color,
       edge.lty = E(union_graph_undir)$lty)
legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1)

#Ned Stark is the character with highest betweenness. 
#This makes sense, as he and his children (specifically Sansa and her arranged marriage to Tyrion) connect to other houses and
#are the central points from which the story unfolds. However, we have to keep in mind here, that my choice of who is important
#enough to include in the network (e.g. the Stark ancestors) and who not (e.g. the whole complicated mess that is the Targaryen and Frey family tree) 
#makes this result somewhat biased.

#### Diameter ####
#In contrast to the shortest path between two nodes, we can also calculate the longest path, or diameter:

diameter(union_graph_undir, directed = FALSE)

#In our network, the longest path connects 21 nodes.

#This, we can also plot:

union_graph_undir_diameter <- union_graph_undir
node_diameter <- get.diameter(union_graph_undir_diameter,  directed = FALSE)

V(union_graph_undir_diameter)$color <- scales::alpha(V(union_graph_undir_diameter)$color, alpha = 0.5)
V(union_graph_undir_diameter)$size <- 2

V(union_graph_undir_diameter)[node_diameter]$color <- "red"
V(union_graph_undir_diameter)[node_diameter]$size <- 5

E(union_graph_undir_diameter)$color <- "grey"
E(union_graph_undir_diameter)$width <- 1

E(union_graph_undir_diameter, path = node_diameter)$color <- "red"
E(union_graph_undir_diameter, path = node_diameter)$width <- 5

plot(union_graph_undir_diameter,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir_diameter)$name),
     vertex.shape = V(union_graph_undir_diameter)$shape,
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     edge.lty = E(union_graph_undir_diameter)$lty)
legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1)




#### PageRank centrality ####
#PageRank (originally used by Google to rank the importance of search results) is similar to eigenvector centrality. 
#Eigenvector centrality scores nodes in a network according to the number of connections to high-degree nodes they have.
#It is therefore a measure of node importance. PageRank similarly considers nodes as more important if they have many incoming edges (or links).

page_rank <- page.rank(union_graph_undir, directed = FALSE)

page_rank_centrality <- data.frame(name = names(page_rank$vector),
                                   page_rank = page_rank$vector) %>%
  mutate(name = as.character(name))

union_characters <- left_join(union_characters, page_rank_centrality, by = "name")

page_rank_centrality %>%
  arrange(-page_rank) %>%
  .[1:10, ]


##### Eigenvector centrality ####

#Matrix representation of a network
#Connections between nodes can also be represented as an adjacency matrix. 
#We can convert our graph object to an adjacency matrix with igraph's as_adjacency_matrix() function. 
#Whenever there is an edge between two nodes, this field in the matrix will get assigned a 1, otherwise it is 0.

adjacency <- as.matrix(as_adjacency_matrix(union_graph_undir))


#We can now calculate the eigenvalues and eigenvectors of the adjacency matrix.

#degree diagonal matrix
degree_diag <- diag(1 / igraph::degree(union_graph_undir))

# PageRank matrix
pagerank <- adjacency %*% degree_diag

eigenvalues <- eigen(pagerank)
#The eigenvector with the highest eigenvalue scores those vertices highly, that have many eges or that are connected to vertices with many edges.

eigenvector <- data.frame(name = rownames(pagerank),
                          eigenvector = as.numeric(eigenvalues$vectors[, which.max(eigenvalues$values)]))

union_characters <- left_join(union_characters, eigenvector, by = "name")

eigenvector %>%
  arrange(eigenvector) %>%
  .[1:10, ]

#Because of their highly connected family ties (i.e. there are only a handful of connections but they are almost all triangles),
#the Greyjoys have been scored with the highest eigenvalues.

#We can find the eigenvector centrality scores with:
eigen_centrality <- igraph::eigen_centrality(union_graph_undir, directed = FALSE)

eigen_centrality <- data.frame(name = names(eigen_centrality$vector),
                               eigen_centrality = eigen_centrality$vector) %>%
  mutate(name = as.character(name))

union_characters <- left_join(union_characters, eigen_centrality, eigenvector, by = "name")

eigen_centrality %>%
  arrange(-eigen_centrality) %>%
  .[1:10, ]

#When we consider eigenvector centrality, Tywin and the core Lannister family score highest.

#### Over all Centerality PLOT ####
#Who are the most important characters?
#We can now compare all the node-level information to decide which characters are the most important in Game of Thrones. 
#Such node level characteristics could also be used as input for machine learning algorithms.

#Let's look at all characters from the major houses:
  
  
union_characters %>%
  filter(!is.na(house2)) %>%
  dplyr::select(-contains("_std")) %>%
  gather(x, y, degree:eigen_centrality) %>%
  ggplot(aes(x = name, y = y, color = house2)) +
  geom_point(size = 3) +
  facet_grid(x ~ house2, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#Taken together, we could say that House Stark (specifically Ned and Sansa)
#and House Lannister (especially Tyrion) are the most important family connections in Game of Thrones  


#### Community Detection ####
g <- union_graph

## check components
components(adjacency)

#decompose into giants
giant <- decompose(g)
giant
#we get two giant components as expected.

#### Walktrap ####
cluster_walktrap(g)

cluster_walktrap(g, steps=10)
cluster_walktrap(g, steps=20)
cluster_walktrap(g, steps=30)


#### Info-map ####

cluster_infomap(union_graph_undir)

#### EDGE betweeness ####

cluster_edge_betweenness(union_graph_undir)


#### Label Propagation ####

cluster_label_prop(union_graph_undir)

##### Louvain Method ####
cluster_louvain(as.undirected(g))


comm <- cluster_louvain(as.undirected(g))
union_characters$cluster <- membership(comm)
unique(union_characters$Label)


union_characters$name[union_characters$cluster==1]
union_characters$name[union_characters$cluster==2]
union_characters$name[union_characters$cluster==3]
union_characters$name[union_characters$cluster==4]
union_characters$name[union_characters$cluster==5]

union_characters$name[union_characters$cluster==6]
union_characters$name[union_characters$cluster==7]
union_characters$name[union_characters$cluster==8]

union_characters$name[union_characters$cluster==9]
union_characters$name[union_characters$cluster==10]
union_characters$name[union_characters$cluster==11]
union_characters$name[union_characters$cluster==12]

union_characters$name[union_characters$cluster==13]


library(dplyr)

community <- union_characters %>% group_by(cluster) %>% 
  summarize(name = paste(sort(unique(name)),collapse=", "))

final = as.data.frame(community)
write.csv("final",file = "community.csv")

#### Exploring Relationship between centerality measures and community ####

# Trying to see if there is a Linear Regression between communities and centrality measures.
# If there is we can model centrality measure as dependent variable and community as predictor
# Centrality Measure  = B0 + B1 * Community

plot(union_characters$cluster,union_characters$degree)

# Here we can see that is no linear relationship hence it cant be modelled

plot(union_characters$cluster,union_characters$closeness)

# With closeness and community Here we can see that is no linear relationship hence it cant be modelled

plot(union_characters$cluster,union_characters$betweenness)


# With Betweeness and community Here we can see that is no linear
#relationship hence it cant be modelled



#### Transitivity ####
#We can calculate the transitivity or ratio of triangles to connected triples for the whole network:

transitivity(union_graph_undir, type = "global")

#Or for each node:
transitivity <- data.frame(name = V(union_graph_undir)$name,
                           transitivity = transitivity(union_graph_undir, type = "local")) %>%
  mutate(name = as.character(name))

union_characters <- left_join(union_characters, transitivity, by = "name")

transitivity %>%
  arrange(-transitivity) %>%
  .[1:10, ]
