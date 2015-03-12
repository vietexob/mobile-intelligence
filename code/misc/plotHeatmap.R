plotHeatmap <- function(trajec.data, mainStr, filename=NULL, colors,
                        no.dendrogram=FALSE, clusterRow=FALSE, clusterCol=FALSE,
                        n.intervals=23, len.interval=30, margins.x=10,
                        margins.y=10, norm.col=FALSE, norm.all=FALSE,
                        symbols=vector(), reverse=FALSE) {
  # n.intervals: number of time intervals
  # len.interval: length (in minutes) of each interval
  require(gplots)
  require(RColorBrewer)
  
  attrIds <- vector()
  if(length(symbols) == 0) {
    attrIds <- names(table(trajec.data$attrId))
  }
  else {
    attrIds <- symbols
  }
  num.attrs <- length(attrIds)
  heatmap.matrix <- matrix(data = 0, nrow = num.attrs, ncol = n.intervals)
  
  th.intervals <- vector() # vector of thresholds
  start <- len.interval
  for(i in 1:n.intervals) {
    th.intervals[i] <- start
    start <- start + len.interval
  }
  
  if(!reverse) {
    rownames(heatmap.matrix) <- attrIds
    colnames(heatmap.matrix) <- as.character(th.intervals)
  }
  else {
    heatmap.matrix <- t(heatmap.matrix)
    rownames(heatmap.matrix) <- as.character(th.intervals)
    
    # Populate the mappings
    attrId.name <- new.env()
    for(i in 1:nrow(day.trajecs)) {
      anAttrId <- day.trajecs$attrId[i]
      attrIdStr <- toString(anAttrId)
      anAttrName <- toString(day.trajecs$attrName[i])
      
      if(is.null(attrId.name[[attrIdStr]])) {
        attrId.name[[attrIdStr]] <- anAttrName
        if(length(attrId.name) == num.attrs) {
          break
        }
      }
    }
    
    # Attraction names for the columns
    attrNames <- vector()
    for(i in 1:num.attrs) {
      attrId <- attrIds[i]
      attrNames[i] <- attrId.name[[attrId]]
#       print(paste(attrId, attrNames[i]))
    }
    colnames(heatmap.matrix) <- attrNames
  }
  
  # Populate the heatmap matrix
  if(!reverse) {
    for(i in 1:nrow(heatmap.matrix)) {
      subset.attrId <- subset(trajec.data, attrId == attrIds[i])
      timeStamp.vector <- subset.attrId$timeStamp
      for(j in 1:length(timeStamp.vector)) {
        aTimeStamp <- timeStamp.vector[j]
        col.index <- which(th.intervals > aTimeStamp)[1]
        heatmap.matrix[i, col.index] <- heatmap.matrix[i, col.index] + 1
      }
    }
  }
  else {
    for(i in 1:ncol(heatmap.matrix)) {
      subset.attrId <- subset(trajec.data, attrId == attrIds[i])
      timeStamp.vector <- subset.attrId$timeStamp
      for(j in 1:length(timeStamp.vector)) {
        aTimeStamp <- timeStamp.vector[j]
        row.index <- which(th.intervals > aTimeStamp)[1]
        heatmap.matrix[row.index, i] <- heatmap.matrix[row.index, i] + 1
      }
    }
  }
  
  # Normalization
  if(norm.all) {
    all.counts <- sum(heatmap.matrix)
    for(i in 1:nrow(heatmap.matrix)) {
      for(j in 1:ncol(heatmap.matrix)) {
        heatmap.matrix[i, j] <- heatmap.matrix[i, j] / all.counts
      }
    }
  }
  else {
    if(!reverse) {
      if(norm.col) { # normalize the columns
        for(j in 1:ncol(heatmap.matrix)) {
          heatmap.matrix[, j] <- heatmap.matrix[, j] / sum(heatmap.matrix[, j])
        }
      }
    }
    else {
      if(norm.col) { # normalize the rows instead
        for(i in 1:nrow(heatmap.matrix)) {
          heatmap.matrix[i, ] <- heatmap.matrix[i, ] / sum(heatmap.matrix[i, ])
        }
      }
    }
  }
  
  print(round(heatmap.matrix, 3))
  if(no.dendrogram) {
    dendroStr <- "none"
  }
  else {
    if(!reverse) {
      dendroStr <- "row"
    }
    else {
      dendroStr <- "col"
    }
  }
  
  scaleStr <- "none"
  angle <- 45
  if(!reverse) {
    scaleStr <- "col"
    xlabStr <- "Mins"
    ylabStr <- "Attractions"
  }
  else {
    scaleStr <- "row"
    xlabStr <- "Attractions"
    ylabStr <- "Mins"
#     angle <- 45
  }
  
  # Plot the heat map
  if(is.null(filename)) {
    filename <- "test.pdf"
  }
  pdf(file = filename)
  heatmap.2(heatmap.matrix, trace = "none", scale = scaleStr, dendrogram = dendroStr,
            col = rev(colors), Rowv = clusterRow, Colv = clusterCol,
            margins = c(margins.x, margins.y), main = mainStr, xlab = xlabStr,
            ylab = ylabStr, srtCol = angle)
  dev.off()
}
