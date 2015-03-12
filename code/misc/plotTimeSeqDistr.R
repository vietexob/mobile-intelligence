plotTimeSeqDistr <- function(timeSeq, mainStr="", x.limit, y.limit,
                             x.label = "", y.label = "", color="blue",
                             is.count=FALSE, plot.axes = TRUE, breaks=50) {
  ## Plot the distribution of timestamps
  
  timeSeq.hist <- hist(timeSeq, breaks = breaks, plot = FALSE)
  x <- timeSeq.hist$breaks
  if(is.count) {
    y <- c(timeSeq.hist$counts, 0)
  }
  else {
    y <- c(timeSeq.hist$density, 0)
  }
  
  if(plot.axes) {
    plot(x, y, type = "s", main = mainStr, col = color,
         xlab = x.label, ylab = y.label, xlim = c(0, x.limit), ylim = c(0, y.limit))
  }
  else {
    plot(x, y, type = "s", main = mainStr, col = color,
         xlab = x.label, ylab = y.label, xlim = c(0, x.limit), ylim = c(0, y.limit),
         axes = FALSE)
  }
}
