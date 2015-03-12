getKullbackLeibler <- function(gen.distr, test.distr) {
  # Computes the KL distance between the two input distributions.
  # gen.distr: the distribution generated from the fitted model
  # test.distr: the empirical distribution of the test set
  # NOTE: This is used to test the divergence between two DISCRETE distributions only.
  
  if(length(gen.distr) != length(test.distr)) {
    stop("Input distributions are not of equal length!")
  }
  
  gen.probDistr <- gen.distr / sum(gen.distr)
  test.probDistr <- test.distr / sum(test.distr)
  KL <- sum(test.probDistr * log2(test.probDistr / gen.probDistr))
  
  return(KL)
}
