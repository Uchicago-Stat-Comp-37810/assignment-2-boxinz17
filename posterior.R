posterior <- function(param){  # define the log posterior function (without normalizing), take parameters as the input
  return (likelihood(param) + prior(param))  # return the sum of log likelihood and log prior
}