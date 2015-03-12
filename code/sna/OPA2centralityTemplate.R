# Coursera SNA optional Programming Assignment 2 template

# load the igraph library
# you may have to install this module if you haven't already
library(igraph)

source("./code/sna/plfit.R")

# read in the graph in GML format
# it is a sampled collection of pages from a strange set of seed categories:
# Math, Sociology, and Chemistry
# Change this to be your local file location
g = read.graph("./data/sna/wikipedia.gml",format="gml")

# obtain summary information about the graph
summary(g)

# obtain the undirected degree distribution (the GML file itself is directed)
degrees = degree(g,mode="all")

# fit the power-law distribution. If $D < 0.05, the Kolmogorov Smirnov test tells 
# us that the distribution is power-law.
# Also, look for the estimated power-law exponent $alpha and $xmin (the point at 
# which you should start fitting the distribution)
# make sure you have executed all the code in plfit.R before running this function
# an explanation is here: http://tuvalu.santafe.edu/~aaronc/powerlaws/
# you can download the file directly here: http://tuvalu.santafe.edu/~aaronc/powerlaws/plfit.r
# You will also need the VGAM R package which you can download via the installer
a = plfit(degrees)
(a)

# plot the cumulative empirical distribution
cumy = c()
y = tabulate(degrees)
x = 1:length(y)
for (i in 1:length(x)) {  
	cumy[i] = sum(y[i:length(x)])/sum(y)
}
options(scipen=10)
plot(x,cumy,log="xy",xlab="degree k",ylab="P(x) >= k",cex=0.5)	
# overlay the fitted distribution
startval = cumy[a$xmin]
fittedvals = (a$xmin:max(x))^(-a$alpha + 1)*(startval)/a$xmin^(-a$alpha + 1) 
points(a$xmin:max(x),fittedvals,type='l',col='red')

# calculate the in and out degrees separately
# use the degree() function and options for calculating directed degree
# see the documentation here: http://igraph.sourceforge.net/documentation.html

# see which nodes have the max out and indegree
# for example, if you were to store the outdegree in the vector od,
# you could look up the page name like so:
od <- degree(g, mode = "out")
V(g)$label[which.max(od)]

# find undirected betweenness scores and then nodes with the max betweenness
# warning, can be slow with large graphs, you may consider betweenness.estimate instead
bb = betweenness(g,directed=F)
V(g)$label[which.max(bb)]

# this high betweennes node may seem a bit surprising
# you can check out its neighbors like this. The +1, -1 business is a real
# pain. It is because R indexes from 1 onward, but igraph likes to number
# its vertices starting with 0. So you have to do a back and forth dance
V(g)$label[V(g)[nei(which.max(bb)-1)]+1]

# calculate PageRank and find the node having the highest pagerank
# you'll want the $vector portion of the answer returned
# the assignment doesn't ask about this, but it's good to know how to do this...
pr = page.rank(g)
V(g)$label[which.max(pr$vector)]

# calculate the Bonacich alpha-centrality of a lattice
# I'm not quite convinced that igraph calculates these correctly,
# but the behavior makes sense to me for these values of alpha
glfour = graph.lattice( c(4,4) )
ac = alpha.centrality(glfour,alpha=-0.5)
plot(glfour,layout=layout.kamada.kawai,vertex.size = 20*ac/(max(ac)-min(ac)))

ac = alpha.centrality(glfour,alpha=+0.25)
plot(glfour,layout=layout.kamada.kawai,vertex.size = 20*ac/(max(ac)-min(ac)))
