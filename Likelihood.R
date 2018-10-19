likelihood <- function(param){  # define the log likelihood, takes the parameters of the distributuion as the input
  a = param[1]  # set a to be the first element of param
  b = param[2]  # set b to be the second element of param
  sd = param[3]  # set standard deviation to be the third element of param
  
  pred = a*x + b  # calculate the predictions (the mean of conditional distribution of y given x, which is actually a normal distribution)
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)  # compute the log likelihood for each data point
  sumll = sum(singlelikelihoods)  # sum all the log likehood for dataset
  return(sumll)  # return the sum
}