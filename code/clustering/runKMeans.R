rm(list = ls())

library(cluster)
library(clValid)

source("../geospatial/earthDist.R")
source("../commons/selectDataByMonth.R")
source("../commons/getEuclideanDistMatrix.R")
source("./getBinMatrix.R")
source("./afterPCA.R")

## For clustering ##
source("../commons/getCollapsedTrajecData.R")
source("../commons/getAnObsVector.R")
source("../commons/fillTheGaps.R")
source("../commons/getActivityMatrix.R")
source("../commons/doHierarchicalClustering.R")

# Read the attractions
filename <- "../../data/attr-coords.csv"
attr.coords <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
dist.matrix <- getEuclideanDistMatrix(attr.coords)

# Select by months
train.month <- c("02", "03")
test.month <- c("04")

# Load the collapsed trajec data for clustering
groups.trajec.data <- read.csv(file = "../../data/collapsed-groups-sales-redemptions.csv",
                               header = TRUE)
solos.trajec.data <- read.csv(file = "../../data/solo-sales-redemptions.csv",
                              header = TRUE)
collapsed.trajec.data <- getCollapsedTrajecData(groups.trajec.data, solos.trajec.data)
# Select the Choice Pass only
is.choicePass <- TRUE
if(is.choicePass) {
  collapsed.trajec.data <- subset(collapsed.trajec.data, IsChoicePass == "true")
}

# Select some months only
monthStr <- c(train.month, test.month)
collapsed.trajec.data <- selectDataByMonth(collapsed.trajec.data, monthStr)
len <- 4
if(len > 0) {
  # Take only those who redeemed len number of attractions
  collapsed.trajec.data <- subset(collapsed.trajec.data, SeqLen == len)
}

startTime <- 0
endTime <- 660 # minutes = 11 hours
interval <- 5
nIntervals <- round((endTime - startTime) / interval)
maxDurations <- attr.coords$Duration
names(maxDurations) <- attr.coords$Id
trajec.matrix <- getActivityMatrix(collapsed.trajec.data, maxDurations, startTime,
                                   endTime, interval)

binMatrix <- getBinMatrix(trajec.matrix)
# Compute the avg activity vector
meanAct <- colMeans(binMatrix)
finalMatrix <- matrix(0, nrow = nrow(trajec.matrix), ncol = ncol(binMatrix))
# Compute the deviation vector for each individual
for(i in 1:ncol(finalMatrix)) {
  finalMatrix[, i] <- binMatrix[, i] - meanAct[i]
}

# Remove all zero cols
# NOTE: This is necessary or the PCA may crash!
nonzero.indicators <- apply(finalMatrix != 0, 2, all)
meanAct <- meanAct[nonzero.indicators]
binMatrix <- binMatrix[, nonzero.indicators]
finalMatrix <- finalMatrix[, nonzero.indicators]

# Perform PCA
print("Performing PCA...")
finalMatrix.pca <- prcomp(finalMatrix, scale. = TRUE, tol = 0)
print("Done.")
# # Print the first two principal components
# print(round(finalMatrix.pca$rotation[1:200, 1:2], 3))

# Save the biplot
pdf(file = "../../figures/k-means/pca-biplot.pdf")
biplot(finalMatrix.pca, cex = 0.4, main = "Biplot")
dev.off()

# Save the scree plot
pdf(file = "../../figures/k-means/pca-scree.pdf")
plot(finalMatrix.pca, type = "lines", main = "Scree Plot")
dev.off()

# Extract the loadings
finalMatrix.pca.rot <- finalMatrix.pca$rotation
# Extract the standard deviations
finalMatrix.pca.sdv <- finalMatrix.pca$sdev

# Choose the number of PC's
epsilon <- 0.025
nPrinComps <- 20
matAdjust <- apply(finalMatrix, 2, function(i) i - mean(i))
meanList <- apply(finalMatrix, 2, mean)

# Reconstruct the deviation matrix
reconstructedMatrix <- afterPCA (
  matAdjust = matAdjust,
  meanList = meanList,
  eigenList = finalMatrix.pca,
  n = nPrinComps,
  specific_select = FALSE
)

# Reconstruct the binary matrix
reconstructedBin <- matrix(0, nrow = nrow(reconstructedMatrix),
                           ncol = ncol(reconstructedMatrix))
for(i in 1:ncol(reconstructedMatrix)) {
  reconstructedBin[, i] <- round(reconstructedMatrix[, i] + meanAct[i])
}
diffMatrix <- abs(binMatrix - reconstructedBin)

diffVector <- c(diffMatrix)
diffVector2 <- diffVector^2
avgDiff <- mean(diffVector2)
avgDiff <- round(avgDiff, 4)
print(paste("Avg. error rate =", avgDiff, "@", nPrinComps))

while(avgDiff > epsilon) {
  nPrinComps <- nPrinComps + 1
  # Reconstruct the deviation matrix
  reconstructedMatrix <- afterPCA (
    matAdjust = matAdjust,
    meanList = meanList,
    eigenList = finalMatrix.pca,
    n = nPrinComps,
    specific_select = FALSE
  )
  # Reconstruct the binary matrix
  reconstructedBin <- matrix(0, nrow = nrow(reconstructedMatrix),
                             ncol = ncol(reconstructedMatrix))
  for(i in 1:ncol(reconstructedMatrix)) {
    reconstructedBin[, i] <- round(reconstructedMatrix[, i] + meanAct[i])
  }
  diffMatrix <- abs(binMatrix - reconstructedBin)
  
  diffVector <- c(diffMatrix)
  diffVector2 <- diffVector^2
  avgDiff <- mean(diffVector2)
  avgDiff <- round(avgDiff, 4)
  print(paste("Avg. error rate =", avgDiff, "@", nPrinComps))
}

# Get the projected/transformed data points
projData <- finalMatrix.pca$x # return the rotated rata
projData <- projData[, 1:nPrinComps]
dissMat <- daisy(projData, "euclidean") # compute the dissimilarity matrix
dissMat <- dissMat^2
# Compute the distance matrix
distMat <- dist(projData, "euclidean")

meanSilVector <- vector()
dunnVector <- vector()
silList <- list()
maxK <- 15
finalK <- 0
finalK_Dunn <- 0

for(k in 1:maxK) {
  # Do K-means clustering
  cl <- kmeans(projData, k)
  
  # Compute the Silhouette index
  # If a Silhouette index of a data point is closer to 1, then it is appropriately clustered;
  # otherwise, if it is closer to -1, then it is badly clustered. A zero index means the vector
  # is borderline of the two natural clusters
  sil <- silhouette(cl$cluster, dissMat)
  avgSil <- NA
  if(length(sil) == 1) { # sil is NA
    sil <- 0
    avgSil <- 0
  }
  
  silList[[k]] <- sil
  if(is.na(avgSil)) {
    avgSil <- mean(sil[, 3]) # take the average silhouette
    avgSil <- round(avgSil, 4)
  }
  
  meanSilVector[k] <- avgSil
  print(paste("avgSil =", avgSil, "@ K =", k))
  
  # Compute the Dunn index
  dunnIndex <- 0
  if(k > 1) {
    dunnIndex <- dunn(distance = distMat, cl$cluster)
    dunnIndex <- round(dunnIndex, 4)
    print(paste("Dunn =", dunnIndex))
    dunnVector[k] <- dunnIndex
  }
  else {
    print(paste("Dunn =", dunnIndex))
    dunnVector[k] <- dunnIndex
  }
  
  if(length(meanSilVector) > 1) {
    if(finalK == 0) {
      curIndex <- meanSilVector[k]
      prevIndex <- meanSilVector[k-1]
      if(curIndex <= prevIndex) {
        finalK <- k-1
        print(paste("finalK =", finalK, "@ avgSil =", meanSilVector[finalK]))
      }
    }
  }
}

pdf(file = "../../figures/k-means/silhouette-indices.pdf")
plot(1:maxK, meanSilVector, xlab = "K", ylab = "Mean Silhouette", type = "l",
     col = "blue", main = "Mean Silhouette Indices")
dev.off()

pdf(file = "../../figures/k-means/dunn-indices.pdf") 
plot(1:maxK, dunnVector, xlab = "K", ylab = "Dunn Indices", type = "l",
     col = "blue", main = "Dunn Indices")
dev.off()

pdf(file = "../../figures/k-means/silhouette.pdf")
plot(silList[[finalK]], main = paste("Silhouette K =", finalK))
dev.off()

# Redo K-means using the final number
final.cl <- kmeans(projData, finalK)
# Save the clusters together with the data frame
collapsed.trajec.data$Label <- final.cl$cluster
filename <- "../../data/k-means.csv"
write.csv(collapsed.trajec.data, file = filename, row.names = FALSE)

# Plot the K-means clustering
pdf(file = "../../figures/k-means/k-means-plot-sil.pdf")
plot(projData, col = final.cl$cluster, main = paste("K =", finalK))
points(final.cl$centers, col = 1:finalK, pch = 8, cex = 2)
dev.off()

# Redo K-means using the final Dunn index
sortedDunnVector <- sort(dunnVector, decreasing = TRUE)
finalK_Dunn <- which(dunnVector == sortedDunnVector[1])[1]
print(paste("Final K (Dunn) =", finalK_Dunn))
final.cl.dunn <- kmeans(projData, finalK_Dunn)

# Plot the K-means clustering (2)
pdf(file = "../../figures/k-means/k-means-plot-dunn.pdf")
plot(projData, col = final.cl.dunn$cluster, main = paste("K =", finalK_Dunn))
points(final.cl.dunn$centers, col = 1:finalK_Dunn, pch = 8, cex = 2)
dev.off()

# Choose the second highest Silhouette index
sortedSilVector <- sort(meanSilVector, decreasing = TRUE)
secondK <- which(meanSilVector == sortedSilVector[2])[1]
print(paste("Second K =", secondK))

# Redo K-means using the second number
second.cl <- kmeans(projData, secondK)
# Plot the second K-means clustering
pdf(file = "../../figures/k-means/k-means-plot-sil-2.pdf")
plot(projData, col = second.cl$cluster, main = paste("K =", secondK))
points(second.cl$centers, col = 1:secondK, pch = 8, cex = 2)
dev.off()

filename <- "../../images/k-means/choice.RData"
save(list = ls(), file = filename)
