rm(list = ls()) # clear the workspace

library(igraph)

## Load the useful functions
source("./code/sna/plfit.R")
source("./code/sna/getGraphObj.R")

## Load the mapping between nodeId and userId
load("./data/sna/userId_map_reverse.RData")

## This dataset was extracted from Stack Overflow (the questioning and answering website).
## Each node in the network is a unique user on the website. There is a directed edge from
## user u to user v if u has answered at least one question posted by v on the website. The
## weight of a directed edge represents the number of questions a user has answered another.
## In this dataset, there are 728 expert users who reply to questions posted by 8,895 users.
weightedEdges <- read.csv(file = "./data/sna/weighted_edges.csv", header = TRUE)

## Construct the igraph object from the CSV file
# g <- getGraphObj(weightedEdges) # alternative way

## First remove the 'freq' column from the matrix
weightedEdges.matrix <- as.matrix(weightedEdges[, -3])
## The graph has 8,908 nodes (or unique users)
g <- graph.edgelist(weightedEdges.matrix, directed = TRUE)
E(g)$weight <- weightedEdges$freq # add weights to the edges

## Name each node with its corresponding userId
all.userIds <- vector()
for(nodeId in 1:length(V(g))) {
  nodeIdStr <- toString(nodeId)
  userIdStr <- userId.map.reverse[[nodeIdStr]]
  
  if(!is.null(userIdStr)) {
    userId <- as.numeric(userIdStr)
    all.userIds[nodeId] <- userId
  }
}
V(g)$userId <- all.userIds

## Print some basic info about the graph -- g is a weighted graph!
summary(g)
## Save the igraph object on disk
save(g, file="./data/sna/stackoverflow_graph.RData")

## Global clustering coefficient of the network
print("Global clustering coefficient of g:")
print(transitivity(g, type="global"))

## Network structure -- degree distributions: all degrees, indegree, and outdegree
degrees <- degree(g, mode = "all")
degrees <- subset(degrees, degrees > 0) # remove all zero degrees
indegrees <- degree(g, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)
outdegrees <- degree(g, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

## Fit the power law to 3 kinds of degree distribution
pl <- plfit(degrees)
pl.in <- plfit(indegrees)
pl.out <- plfit(outdegrees)
print("Overall degree distribution:")
print(pl) # test if it is power-lawed and print its alpha exponent
print("Indegree distribution:")
print(pl.in)
print("Outdegree distribution:")
print(pl.out)

## Plot the cumulative empirical distribution
cumy <- c() # empty, expandable vector
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
## For indegrees
cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
## For outdegrees
cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

## For all degrees
pdf(file = "./figures/sna/all_deg_dist_pl.pdf")
op <- par(mfrow = c(2, 1))
options(scipen = 10)
## Histogram and cumulative distribution function
hist(degrees, freq = FALSE, xlab = "Degree k",
     main = "Histogram of All Degrees", breaks = 50, col = "gray")
mainStr <- "Degree Power-law Distribution"
plot(x, cumy, log = "xy", xlab = "Degree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr)
## Overlay the fitted distribution
startval <- cumy[pl$xmin]
fittedvals <- (pl$xmin:max(x))^(-pl$alpha + 1) * (startval) / pl$xmin^(-pl$alpha + 1)
points(pl$xmin:max(x), fittedvals, type = 'l', col = 'red')
## Paste value of alpha onto the plot
alpha <- pl$alpha
text(80, .80, labels = bquote(paste(alpha, " = ", .(alpha))), col = "red", pos = 1)
dev.off()

## For indegrees and outdegrees
pdf(file = "./figures/sna/in_out_deg_dist_pl.pdf")
op <- par(mfrow = c(2, 2))
options(scipen = 10)

## Histogram and cumulative distribution function
hist(indegrees, freq = FALSE, xlab = "Indegree k",
     main = "Histogram of Indegrees", breaks = 50, col = "gray")
mainStr.in <- "Indegree Power-law Distr."
plot(x.in, cumy.in, log = "xy", xlab = "Indegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr.in)

## Overlay the fitted distribution
startval.in <- cumy.in[pl.in$xmin]
fittedvals.in <- (pl.in$xmin:max(x))^(-pl.in$alpha + 1) * (startval.in) / pl.in$xmin^(-pl.in$alpha + 1)
points(pl.in$xmin:max(x), fittedvals.in, type = 'l', col = 'red')

## Paste value of alpha onto the plot
alpha.in <- pl.in$alpha
text(60, .50, labels = bquote(paste(alpha, " = ", .(alpha.in))), col = "red", pos = 1)

## Histogram and cumulative distribution function
hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
     main = "Histogram of Outdegrees", breaks = 50, col = "gray")
mainStr.out <- "Outdegree Power-law Distr."
plot(x.out, cumy.out, log = "xy", xlab = "Outdegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr.out)

## Overlay the fitted distribution
startval.out <- cumy.out[pl.out$xmin]
fittedvals.out <- (pl.out$xmin:max(x))^(-pl.out$alpha + 1) * (startval.out) / pl.out$xmin^(-pl.out$alpha + 1)
points(pl.out$xmin:max(x), fittedvals.out, type = 'l', col = 'red')

## Paste value of alpha onto the plot
alpha.out <- pl.out$alpha
text(80, .80, labels = bquote(paste(alpha, " = ", .(alpha.out))), col = "red", pos = 1)

## Save the plots and reset to the previous settings
dev.off()
par(op)
