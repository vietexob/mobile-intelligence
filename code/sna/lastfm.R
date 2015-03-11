# Load the igraph and foreign packages (install if needed)
# Edited. See original: http://www.r-bloggers.com/music-network-visualization/

require(igraph)
require(foreign)

lastfm<-read.csv("./data/sna/lastfm_network_ad.csv", header=T,
                 encoding="UTF-8") #Load the dataset

lastfm$include<-ifelse(lastfm$Similar %in% lastfm$Artist==T,1,0) #Index the links between artists in the library
lastfm.network<-graph.data.frame(lastfm, directed=F) #Import as a graph

last.attr<-lastfm[-which(duplicated(lastfm$Artist)),c(5,3,4) ] #Create some attributes
V(lastfm.network)[1:106]$listeners<-last.attr[,2]
V(lastfm.network)[107:length(V(lastfm.network))]$listeners<-NA
V(lastfm.network)[1:106]$tag<-last.attr[,3]
V(lastfm.network)[107:length(V(lastfm.network))]$tag<-NA #Attach the attributes to the artist from the library (only)
V(lastfm.network)$label.cex$tag<-ifelse(V(lastfm.network)$listeners>1200000, 1.4, 
                                        (ifelse(V(lastfm.network)$listeners>500000, 1.2,
                                                (ifelse(V(lastfm.network)$listeners>100000, 1.1,
                                                        (ifelse(V(lastfm.network)$listeners>50000, 1, 0.8))))))) #Scale the size of labels by the relative popularity

V(lastfm.network)$color<-"blue" #Set the color of the dots
V(lastfm.network)$size<-0.1 #Set the size of the dots
V(lastfm.network)$label.color<-NA
V(lastfm.network)[1:106]$label.color<-"blue" #Only the artists from the library should be in white, the rest are not needed

E(lastfm.network)[ include==0 ]$color<-"black" 
E(lastfm.network)[ include==1 ]$color<-"red" #Color edges between artists in the library red, the rest are not needed

# fix(tkplot) # Add manually to the function an argument for the background color of the canvas and set it to black (bg=black)
# Plot the graph and adjust as needed
plot(lastfm.network, vertex.label=V(lastfm.network)$name, layout=layout.fruchterman.reingold)
