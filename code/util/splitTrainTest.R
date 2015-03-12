splitTrainTest <- function(data, k, unique.key="", test.id=1) {
  # data: the input data
  # k: the number of folds
  
  if(k < 2) {
    stop(paste("k = ", k, sep = ""))
  }
  if(nchar(unique.key) == 0) {
    stop("unique.key not specified!")
  }
  
  uniqueKey.names <- names(table(data[, unique.key]))
  ids <- sample(1:k, length(uniqueKey.names), replace = TRUE)
  fold.list <- 1:k
  
  training.uniqueKeys <- subset(uniqueKey.names, ids %in% fold.list[-test.id])
  training.indicators <- data[, unique.key] %in% training.uniqueKeys
  data$IsTest <- !training.indicators
  
  return(data)
}
