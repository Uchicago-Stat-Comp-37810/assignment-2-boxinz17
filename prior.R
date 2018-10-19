# Prior distribution
prior <- function(param){  # define the log prior, take parameters as the input
  a = param[1]  # set a to be the first element of param
  b = param[2]  # set b to be the second element of param
  sd = param[3]  # set standard deviation to be the third element of param
  aprior = dunif(a, min=0, max=10, log = T)  # define the prior distribution for a as the uniform distribution between 0 and 10
  bprior = dnorm(b, sd = 5, log = T)  # define the prior distribution for b as the norm distribution with mean 0 and standard deviation 5
  sdprior = dunif(sd, min=0, max=30, log = T)  # define the prior distribtuion for sd as the uniform distribution between 0 and 30
  return(aprior+bprior+sdprior)  # return the sum of log prior for three variables
}