getRMSError <- function(gen.data, test.data, is.discrete) {
  # Returns the root-mean-square error of the two distributions
  RMSE <- 0
  if(is.discrete) {
    # Expects the two input data are frequency tables
    if(length(gen.data) != length(test.data)) {
      stop("gen.table and test.table do not have the same length!")
    }
    
    # Covert into probabilities
    gen.data <- gen.data / sum(gen.data)
    test.data <- test.data / sum(test.data)
    
    RMSE <- mean((gen.data - test.data)^2)
    RMSE <- sqrt(RMSE)
  }
  else { # is continuous
#     # Expect two input vectors of data of equal length
#     if(length(gen.data) != length(test.data)) {
#       stop("gen.data and test.data do not have the same length!")
#     }
    
    gen.hist <- hist(gen.data, plot = FALSE, breaks = 50)
    gen.hist.breaks <- gen.hist$breaks
    test.hist <- hist(test.data, plot = FALSE, breaks = 50)
    test.hist.breaks <- test.hist$breaks
    counter <- 0
    
    for(i in 1:(length(test.hist.breaks)-1)) { # reality
      test.dens <- test.hist$density[i]
      to.test <- test.hist.breaks[i+1]
      
      if(length(test.dens) > 0) {
        for(j in 1:length(gen.hist.breaks)) { # generated
          to.gen <- gen.hist.breaks[j]
          if(to.gen >= to.test) {
            gen.dens <- gen.hist$density[j-1]
            if(length(gen.dens) > 0) {
              RMSE <- RMSE + (test.dens - gen.dens)^2
              
            }
            else { # no value found
              RMSE <- RMSE + test.dens^2
            }
            
            counter <- counter + 1
            break
          }
          
          if(j == length(gen.hist.breaks)) { # no obs in the generated interval
            RMSE <- RMSE + test.dens^2
            counter <- counter + 1
          }
        }
      }
    }
    
    if(counter == 0) {
    	stop(paste("counter =", counter))
    }
    
    RMSE <- sqrt((RMSE / counter))
  }
  
  RMSE <- round(RMSE, 4)
  return(RMSE)
}
