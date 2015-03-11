# Author: Jeff Hemsley jhemsley at uw dot edu
# Created: Nov 20 2012

# File generates random graph with random dates and creates set of pngs for animation
library(igraph)

# igraph has many nifty ways to generate random graphs. :-)
start.nodes <- 2
total.nodes <- 500
g <- erdos.renyi.game(start.nodes, 1/2, directed=T)
g <- barabasi.game(total.nodes, start.graph=g, out.pref=T, directed=T,
                   out.seq=rep(2, total.nodes - start.nodes))

# make a layout and set the x & y attributes of the graph vertices 
l <- layout.fruchterman.reingold(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]

# since I'm using a random graph, i want to use random dates for this example
start.date <- as.POSIXct(strptime('2012-01-01 07:00:00', '%Y-%m-%d %H:%M:%S'))
end.date <- as.POSIXct(strptime('2012-01-07 07:00:00', '%Y-%m-%d %H:%M:%S'))
possible.dates <- seq.POSIXt(start.date, end.date, by="hour")
num.time.steps <- length(possible.dates) # we use later for the loop.

# now we need to associate dates with links and I use sample with replace=T
num.edges <- ecount(g)
E(g)$date <- sample(possible.dates, num.edges, replace=T)
E(g)$width <- 2

# these are some initial edge and vertex settings. 
# Note that I set a value for red, green and blue between 0 and 255,
# an alpha, or transparency value of 0 making all objects transparent
E(g)$red <- 255
E(g)$green <- 140
E(g)$blue <- 0
E(g)$alpha <- 0
# then I give a default color just so the attribute exists for later.
E(g)$color <- "black"

V(g)$red <- 95
V(g)$green <- 158
V(g)$blue <- 160
V(g)$alpha <- 0
V(g)$color <- "black"

# season to taste
V(g)$size <- 5
V(g)$frame.color <- NA
V(g)$label <- ""

# in this example I am using a look back of 12 frames for the fade out
# so, over 12 movie frames the links and vertices get more and more
# transparent. 
look.back.default <- 12
alpha.vec.default <- round(seq(0, 255, length=look.back.default + 1),0)
alpha.vec.length <- length(alpha.vec.default)

# workhorse loop
for (time.step in 1:num.time.steps) {
  
  # look.back needs to be altered at the early part of the animation
  look.back <- time.step - look.back.default
  if (look.back < 0) {
    look.back <- 1
  }
  
  date.fade.index <- look.back:time.step
  date.fade.index.length <- length(date.fade.index)
  
  # we always want to set the newest edge/vertex alpha last and we 
  # we always want it to be opaque. But if look.back is greater than
  # available time steps we need to shorten the alpha vector
  alpha.vec <- alpha.vec.default
  if ((alpha.vec.length - date.fade.index.length) > 0) {
    alpha.vec <- alpha.vec[-(1:(alpha.vec.length - date.fade.index.length))]
  }
  
  # for each look.back time step we set alpha for edges/vertices
  # with those time stamps. Some time steps may have no links or
  # vertices, some have many. Do the newest last so that they don't
  # get down-graded if they show up more than once in the fade out 
  # period
  for (j in 1:length(date.fade.index)) {
    active.edges <- which(E(g)$date == possible.dates[date.fade.index[j]])
    
    if (length(active.edges) > 0) {
      E(g)[active.edges]$alpha <- alpha.vec[j]
      V(g)[from(E(g)[active.edges])]$alpha <- alpha.vec[j]
      V(g)[to(E(g)[active.edges])]$alpha <- alpha.vec[j]
    }
  }  
  
  # now make sure all edge/vertext colors are set with whatever their alphas are
  E(g)$color <- rgb(red=E(g)$red, green=E(g)$green
                    , blue=E(g)$blue, maxColorValue=255, alpha=E(g)$alpha)
  V(g)$color <- rgb(V(g)$red, V(g)$green, V(g)$blue, V(g)$alpha, maxColorValue=255)
  
  # file names should be the same except with an incremented number. 
  # many other R-animation authors use ffmpeg 
  out.file.name <- paste("./figures/NetAnimation_", time.step, ".png", sep="")
  png(out.file.name, width=640, height=480)
  plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0, edge.curved = .5, main="")
  dev.off()
}
