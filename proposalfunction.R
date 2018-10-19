proposalfunction <- function(param){  # define the proposal function, take parameters as the input
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))  # define the propsal function as a normal distribution, take the parameter states now as the mean of the distribution
}