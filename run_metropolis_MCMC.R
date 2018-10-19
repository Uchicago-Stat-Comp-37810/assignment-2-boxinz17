run_metropolis_MCMC <- function(startvalue, iterations){  # define the MH sampling function, take start value(for three parameters) and number for iteration as the input
  chain = array(dim = c(iterations+1,3))  # initialize the chain as a (iteration+1) * 3 array, to record the sampling in each iteration
  chain[1,] = startvalue  # set the first row of chain to be startvalue
  for (i in 1:iterations){  # use the for loop to draw samples
    proposal = proposalfunction(chain[i,])  # use proposal function to draw (i+1)th sample based on the ith sample
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))  # calculate the acceptance probability. Because the symmetry property of the normal distribution, the proposal probability just cancelled. We only need to calculate the the ratio of target distribution.
    if (runif(1) < probab){  # use a uniform random variable between 0 and 1 to decide whether to accept the sample or not, so that the acceptance rate will be the desired probability
      chain[i+1,] = proposal  # if the uniform random variable is smaller than the acceptance probability, then we accept this sample and put it into chain
    }else{
      chain[i+1,] = chain[i,]  # otherwise, we drop this sample, set the (i+1)th sample the same as the ith sample
    }
  }
  return(chain)  # after iterating for setted times, we return the whole chain
}