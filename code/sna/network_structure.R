rm(list = ls())

source("./code/sna/plfit.R")

load("./data/sna/userId_map_reverse.RData")

weightedEdges <- read.csv(file = "./data/sna/weighted_edges.csv", header = TRUE)

# Construct the graph object
weightedEdges.matrix <- as.matrix(weightedEdges[, -3]) # remove the 'freq' column from the matrix
g <- graph.edgelist(weightedEdges.matrix, directed = TRUE)
E(g)$weight <- weightedEdges$freq # add weights to the edges

# Add userId to the nodes
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

# Degree Distributions Using iGraph -- all degrees, indegree, and outdegree
summary(g) # print some basic info about the graph -- g is a weighted graph!
# Obtain the undirected degree distribution
degrees <- degree(g, mode = "all")
degrees <- subset(degrees, degrees != 0) # remove all zero degrees
indegrees <- degree(g, mode = "in")
indegrees <- subset(indegrees, indegrees != 0)
outdegrees <- degree(g, mode = "out")
outdegrees <- subset(outdegrees, outdegrees != 0)

# Fit the power-law distribution
pl <- plfit(degrees)
pl.in <- plfit(indegrees)
pl.out <- plfit(outdegrees)
print("Overall degree distribution:")
print(pl) # test if it is power-lawed and print its alpha exponent
print("Indegree distribution:")
print(pl.in)
print("Outdegree distribution:")
print(pl.out)

# Plot the cumulative empirical distribution
cumy <- c() # empty, expandable vector
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
# For indegrees
cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
# For outdegrees
cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

# For all degrees
pdf(file = "./figures/sna/all_deg_dist_pl.pdf")
op <- par(mfrow = c(2, 1))
options(scipen = 10)
# Histogram & cumulative dist function
hist(degrees, freq = FALSE, xlab = "Degree k", main = "Histogram of All Degrees", breaks = 50, col = "gray")
mainStr <- "Degree Power-law Distribution"
plot(x, cumy, log = "xy", xlab = "Degree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr)
# Overlay the fitted distribution
startval <- cumy[pl$xmin]
fittedvals <- (pl$xmin:max(x))^(-pl$alpha + 1) * (startval) / pl$xmin^(-pl$alpha + 1)
points(pl$xmin:max(x), fittedvals, type = 'l', col = 'red')
# Paste value of alpha onto the plot
alpha <- pl$alpha
text(80, .80, labels = bquote(paste(alpha, " = ", .(alpha))), col = "red", pos = 1)
dev.off()

# For in- and outdegrees
pdf(file = "./figures/sna/in_out_deg_dist_pl.pdf")
op <- par(mfrow = c(2, 2))
options(scipen = 10)

# Histogram & cumulative dist function
hist(indegrees, freq = FALSE, xlab = "Indegree k", main = "Histogram of Indegrees", breaks = 50, col = "gray")
mainStr.in <- "Indegree Power-law Distr."
plot(x.in, cumy.in, log = "xy", xlab = "Indegree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr.in)

# Overlay the fitted distribution
startval.in <- cumy.in[pl.in$xmin]
fittedvals.in <- (pl.in$xmin:max(x))^(-pl.in$alpha + 1) * (startval.in) / pl.in$xmin^(-pl.in$alpha + 1)
points(pl.in$xmin:max(x), fittedvals.in, type = 'l', col = 'red')

# Paste value of alpha onto the plot
alpha.in <- pl.in$alpha
text(60, .50, labels = bquote(paste(alpha, " = ", .(alpha.in))), col = "red", pos = 1)

# Histogram & cumulative dist function
hist(outdegrees, freq = FALSE, xlab = "Outdegree k", main = "Histogram of Outdegrees", breaks = 50, col = "gray")
mainStr.out <- "Outdegree Power-law Distr."
plot(x.out, cumy.out, log = "xy", xlab = "Outdegree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr.out)

# Overlay the fitted distribution
startval.out <- cumy.out[pl.out$xmin]
fittedvals.out <- (pl.out$xmin:max(x))^(-pl.out$alpha + 1) * (startval.out) / pl.out$xmin^(-pl.out$alpha + 1)
points(pl.out$xmin:max(x), fittedvals.out, type = 'l', col = 'red')

# Paste value of alpha onto the plot
alpha.out <- pl.out$alpha
text(80, .80, labels = bquote(paste(alpha, " = ", .(alpha.out))), col = "red", pos = 1)

# Save and reset to the previous settings
dev.off()
par(op)
