library(sna)
library(dplyr)
library(ggplot2)

# Relational Data: Set URLs and download data
book1_edges <- "https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book1-edges.csv"
book1_nodes <- "https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book1-nodes.csv"
book2_edges <- "https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book2-edges.csv"
book2_nodes <- "https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book2-nodes.csv"
book3_edges <- "https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book3-edges.csv"
book3_nodes <- "https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book3-nodes.csv"

## Download all data
b1_edges <- read.csv(book1_edges)
b1_nodes <- read.csv(book1_nodes)
b2_edges <- read.csv(book2_edges)
b2_nodes <- read.csv(book2_nodes)
b3_edges <- read.csv(book3_edges)
b3_nodes <- read.csv(book3_nodes)

## Edges
b1_edges$Source <- as.character(b1_edges$Source)
b1_edges$Target <- as.character(b1_edges$Target)
b2_edges$Source <- as.character(b2_edges$Source)
b2_edges$Target <- as.character(b2_edges$Target)
b3_edges$Source <- as.character(b3_edges$Source)
b3_edges$Target <- as.character(b3_edges$Target)

## Nodes
b1_allch <- c(as.character(b1_edges$Source), as.character(b1_edges$Target))
b1_char <- unique(b1_allch)
b1_nchar <- length(b1_char)
b2_allch <- c(as.character(b2_edges$Source), as.character(b2_edges$Target))
b2_char <- unique(b2_allch)
b2_nchar <- length(b2_char)
b3_allch <- c(as.character(b3_edges$Source), as.character(b3_edges$Target))
b3_char <- unique(b3_allch)
b3_nchar <- length(b3_char)

# Create and populate the adjacency matrix
b1_matrix <- matrix(0, b1_nchar, b1_nchar)
rownames(b1_matrix) <- b1_char
colnames(b1_matrix) <- b1_char
b1_weights <- matrix(0, b1_nchar, b1_nchar)
rownames(b1_weights) <- b1_char
colnames(b1_weights) <- b1_char
for(i in 1:nrow(b1_edges)){
  b1_weights[b1_edges[i, 1], b1_edges[i, 2]] <- b1_edges[i, 4]
  b1_matrix[b1_edges[i, 1], b1_edges[i, 2]] <- 1
}

b2_matrix <- matrix(0, b2_nchar, b2_nchar)
rownames(b2_matrix) <- b2_char
colnames(b2_matrix) <- b2_char
b2_weights <- matrix(0, b2_nchar, b2_nchar)
rownames(b2_weights) <- b2_char
colnames(b2_weights) <- b2_char
for(i in 1:nrow(b2_edges)){
  b2_weights[b2_edges[i, 1], b2_edges[i, 2]] <- b2_edges[i, 4]
  b2_matrix[b2_edges[i, 1], b2_edges[i, 2]] <- 1
}

b3_matrix <- matrix(0, b3_nchar, b3_nchar)
rownames(b3_matrix) <- b3_char
colnames(b3_matrix) <- b3_char
b3_weights <- matrix(0, b3_nchar, b3_nchar)
rownames(b3_weights) <- b3_char
colnames(b3_weights) <- b3_char
for(i in 1:nrow(b3_edges)){
  b3_weights[b3_edges[i, 1], b3_edges[i, 2]] <- b3_edges[i, 4]
  b3_matrix[b3_edges[i, 1], b3_edges[i, 2]] <- 1
}

## Make Adjacency matrix symmetric
#b1_matrix <- b1_matrix + t(b1_matrix)
#paste("Adjacency Matrix is Symmetric: ",isSymmetric(b1_matrix))
#b1_weights <- b1_weights + t(b1_weights)
#paste("Weighted Adjacency Matrix is Symmetric: ",isSymmetric(b1_weights))
## --> seems unneccessary

## Subset only people present in all three books
a <- rownames(b1_matrix)
b <- rownames(b2_matrix)
c <- rownames(b3_matrix)
ab <- intersect(a, b)
bc <- intersect(b, c)
names_subsetting <- intersect(ab, bc)
rm(list = c("a", "b", "c", "ab", "bc"))

b1_sub <- b1_matrix[names_subsetting, names_subsetting]
b2_sub <- b2_matrix[names_subsetting, names_subsetting]
b3_sub <- b3_matrix[names_subsetting, names_subsetting]


library(igraph)

graph1 <- graph.adjacency(b1_sub)
graph2 <- graph.adjacency(b2_sub)
graph3 <- graph.adjacency(b3_sub)
graph123 <- graph.adjacency(b1_sub + b2_sub + b3_sub)
myLayout <- layout.fruchterman.reingold(graph123)

# Plotting the networks without attributes
par(mfrow = c(1, 3))
plot(graph1,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Book 1")
plot(graph2,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Book 2")
plot(graph2,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Book 3")
par(mfrow = c(1, 1))

# Additional Data
meta_data <- read.csv("https://query.data.world/s/vF1uVVl9L-dTeDQfVrwsJKZxpmRt0Q", header=TRUE, stringsAsFactors=FALSE);

library(fuzzyjoin)
nameslist <- as.data.frame(rownames(b1_matrix))
names(nameslist) <- "Name"
meta_data_sel <- stringdist_left_join(nameslist, meta_data, by = "Name", max_dist = 1)

meta_data_sel <- filter(meta_data_sel, !is.na(Name.y))

mistakes <- c(3, 88, 150)
### 
# Filtering out fuzzy merging mistakes
# > meta_data_sel[mistakes,]
#     Name.x Name.y   Allegiances Death.Year Book.of.Death Death.Chapter Book.Intro.Chapter
#3     Aggo   Iggo          None        299             3             3                  3
#88    Mord   Moro          None         NA            NA            NA                  3
#150   Joss   Koss    Night's Watch     299             2            14                  5
###
meta_data_sel <- meta_data_sel[-mistakes,]

###
# Recoding House Allegiances -- imperfect coding
# e.g. 'Targaryen' and 'House Tergaryen' --> remove 'House '
##
meta_data_sel$Allegiances <- gsub("House ", "", meta_data_sel$Allegiances)

b1_MMatrix <- b1_matrix[intersect(meta_data_sel$Name.x, rownames(b1_matrix)),intersect(meta_data_sel$Name.x, rownames(b1_matrix))]
b1_Meta <- meta_data_sel[meta_data_sel$Name.x == intersect(meta_data_sel$Name.x, rownames(b1_matrix)),]

graph1 <- graph.adjacency(b1_MMatrix)
myLayout <- layout.fruchterman.reingold(graph1)
png("homophily_plots.png", 1800, 1000)
par(mfrow= c(1,3))
plot(graph1,
     vertex.color = ifelse(b1_Meta$Gender == 1, "blue", "red"),
     vertex.shape = ifelse(b1_Meta$Gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Gender Homophily")

legend(x=-1.5, y=-1.1, c("Male","Female"), pch=c(22, 21),
       col="#777777", pt.bg=c("blue", "red"), pt.cex=2, cex=.8, bty="n", ncol=1)

plot(graph1,
     vertex.color = factor(b1_Meta$Allegiances),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Allegiances")

plot(graph1,
     vertex.color = ifelse(b1_Meta$Nobility == 1, "orange", "grey"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Nobility")

legend(x=-1.5, y=-1.1, c("Nobility","Commoner"), pch = 21,
       col="#777777", pt.bg=c("orange", "grey"), pt.cex = 2, cex=.8, bty="n", ncol=1)

par(mfrow= c(1,1))
dev.off()

detach(package:igraph)




### Social Network Modelling ###
library(statnet)
library(sna)

## Set up the Network
got_b1 <- network(b1_MMatrix, directed = F)
got_b1 %v% "sex" <- b1_Meta$Gender
got_b1 %v% "nobility" <- b1_Meta$Nobility
got_b1 %v% "allegiances" <- b1_Meta$Allegiances


## Centrality Measures by House ##
n.verts <- nrow(b1_MMatrix)

max.degree <- n.verts - 1
cent_degree <- degree(got_b1, gmode = "graph") / max.degree # gmode = "graph" --> undirected network

max.betweenness <- (n.verts - 1) * (n.verts - 2) / 2
cent_betw <- betweenness(got_b1, gmode="graph") / max.betweenness

centrality.mat <- cbind(cent_degree, cent_betw)
colnames(centrality.mat) <- c("degree" , "betweenness" ) 
centrality.mat
cent_house <- as.data.frame(centrality.mat)
cent_house <- cbind(cent_house, house = b1_Meta$Allegiances)

# Final Centrality Table
cent_house %>%
  group_by(house) %>%
  summarize(meanDegree = mean(degree),
            meanBetweenness = mean(betweenness)) %>%
  arrange(desc(meanDegree))
  

#### ERGM ####

## Empty Model
ergm1 <- ergm(got_b1 ~ edges)
summary(ergm1)
# = gden(got_b1)

## Clustering (number of third nodes connecting A --> B)
ergm2 <- ergm(got_b1 ~ edges + gwesp(0, fixed = T))
mcmc.diagnostics(ergm2)
summary(ergm2)
ergm2_gof <- gof(ergm2)
plot(ergm2_gof)
## gwesp -- counts the number of third nodes that connect nodes A --> B
## --> how does the probability of a tie change if it is embedded in X number of triangles
## p.s. we often take the sqrt() of the number of triangles (decreasing impact of number of embedded triads
## ==> it is non-linear) -- this is regulated by the 'decay' argument -- if you want decay, set it to
## a non-negative value

# Adding Node Covariates
ergm3 <- ergm(got_b1 ~ edges + gwesp(0, fixed = T) 
              + nodecov("sex") + nodecov("nobility") + nodefactor("allegiances"))
mcmc.diagnostics(ergm3)
summary(ergm3)
ergm3_gof <- gof(ergm3)
plot(ergm3_gof)

# Adding Nobility


# Largest Model
ergm6 <- ergm(got_b1 ~ edges + gwesp(0, fixed = T) 
              + nodecov("sex") + nodecov("nobility") + nodefactor("allegiances"))
mcmc.diagnostics(ergm6)
summary(ergm6)

ergm6_gof <- gof(ergm6)
plot(ergm6_gof)

plot(gof(ergm6, GOF = ~ degree + esp + distance))

# Adding popularity to the model
ergm7 <- ergm(got_b1 ~ edges + gwesp(0, fixed = T) 
                     + nodecov("sex") + nodecov("nobility") + nodefactor("allegiances") +
                     + nodematch("sex") + nodematch("nobility") + nodematch("allegiances"))
mcmc.diagnostics(ergm7)
summary(ergm7)

## Export the models
library(broom)
sum1 <- tidy(ergm1)
sum1 <- sum1[, 1:3]
sum2 <- tidy(ergm2)
sum2 <- sum2[, 1:3]
sum3 <- tidy(ergm3)
sum3 <- sum3[, 1:3]
sum6 <- tidy(ergm6)
sum6 <- sum6[, 1:3]
sum7 <- tidy(ergm7)
sum7 <- sum7[, 1:3]

sum7 %>%
  left_join(sum6, by = "term") %>%
  left_join(sum3, by = "term") %>%
  left_join(sum2, by = "term") %>%
  left_join(sum1, by = "term") -> sum_fin

write.table(sum_fin, "final_models.txt")

              