plotPieChart <- function(slices, labels, mainStr) {
  pcts <- round((slices / sum(slices)) * 100, 2)
  labels <- paste(labels, pcts)
  labels <- paste(labels, "%", sep = "")
  pie(slices, labels = labels, main = mainStr)
}
