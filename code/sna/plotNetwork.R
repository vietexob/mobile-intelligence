plotNetwork <- function(g, mainStr, filename, edgeWeight.theta=0) {
  V(g)$size <- degree(g, mode = "out")
  max.weight <- max(E(g)$weight)
  weight.factor <- max.weight / 5
  
  if(edgeWeight.theta == 0) {
    summ.edgeWeight <- summary(E(g)$weight)
    edgeWeight.theta <- exp(1) * summ.edgeWeight[3] # the median
  }
  
  E(g)$color <- ifelse(E(g)$weight >= edgeWeight.theta, 'darkorange', 'azure4')
  E(g)$width <- E(g)$weight / weight.factor
  E(g)$arrow.width <- E(g)$width / 2
  
  # Plot & save the network
  pdf(file = filename)
  par(mai = c(0, 0, 1, 0)) # reduce the sizes of the margins
  
  plot(g, # the network to be plotted
       layout = layout.fruchterman.reingold, # the layout method
       main = mainStr, # specifies the title
       vertex.label.dist = 0,  # puts the name labels slightly off the dots
       # vertex.frame.color = 'blue', # the color of the border of the dots 
       vertex.label.color = 'black', # the color of the name labels
       vertex.label.font = 2,  	# the font of the name labels
       vertex.label = V(g)$name,	# specifies the lables of the vertices
       vertex.label.cex = 1,		# specifies the size of the font of the labels
       edge.curved = TRUE
  )
  
  dev.off()
}
