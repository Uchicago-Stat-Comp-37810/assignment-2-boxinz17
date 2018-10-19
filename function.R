# likelihood function
likelihood <- function(param){  # define the log likelihood, takes the parameters of the distributuion as the input
  a = param[1]  # set a to be the first element of param
  b = param[2]  # set b to be the second element of param
  sd = param[3]  # set standard deviation to be the third element of param
  
  pred = a*x + b  # calculate the predictions (the mean of conditional distribution of y given x, which is actually a normal distribution)
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)  # compute the log likelihood for each data point
  sumll = sum(singlelikelihoods)  # sum all the log likehood for dataset
  return(sumll)  # return the sum
}

slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}  # use true value of b and sd, take a as the only varibale, return the likelihhod function depending on a

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

# Posterior function
posterior <- function(param){ # define the log posterior function (without normalizing), take parameters as the input
  return (likelihood(param) + prior(param))  # return the sum of log likelihood and log prior
}

# Proposal function
proposalfunction <- function(param){  # define the proposal function, take parameters as the input
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))  # define the propsal function as a normal distribution, take the parameter states now as the mean of the distribution
}

# run Metropolis algorithm function
run_metropolis_MCMC <- function(startvalue, iterations){  # define the MH sampling function, take start value(for three parameters) and number for iteration as the input
  chain = array(dim = c(iterations+1,3))  # initialize the chain as a (iteration+1) * 3 array, to record the sampling in each iteration
  chain[1,] = startvalue  # set the first row of chain to be startvalue
  for (i in 1:iterations){  # use the for loop to draw samples
    proposal = proposalfunction(chain[i,])  # use proposal function to draw (i+1)th sample based on the ith sample
    
    if(is.infinite(posterior(proposal))){  # if the log posterior is negative infinite, just set prob as 0
      probab <- 0
    } else {
      probab <- exp(posterior(proposal) - posterior(chain[i,]))  # calculate the acceptance probability. Because the symmetry property of the normal distribution, the proposal probability just cancelled. We only need to calculate the the ratio of target distribution.
    }
    if (runif(1) < probab){  # use a uniform random variable between 0 and 1 to decide whether to accept the sample or not, so that the acceptance rate will be the desired probability
      chain[i+1,] <- proposal  # if the uniform random variable is smaller than the acceptance probability, then we accept this sample and put it into chain
    }else{
      chain[i+1,] <- chain[i,]  # otherwise, we drop this sample, set the (i+1)th sample the same as the ith sample
    }
  }
  return(chain)  # after iterating for setted times, we return the whole chain
}

# summary function
MHSummary <- function(chain, burnIn, trueA, truB, trueSd){
  par(mfrow = c(2,3))  # set the graph to contain 2 * 3 subgraphs
  hist(chain[-(1:burnIn),1],nclass=30, main="Posterior of a", xlab="True value = red line" )  # draw the histogram of kept sampling of a
  abline(v = mean(chain[-(1:burnIn),1]))  # draw a vertical line indicating the mean value of sampled a
  abline(v = trueA, col="red" )  # draw a red vertical line indicating the true value of a
  hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")  # draw the histogram of kept sampling of b
  abline(v = mean(chain[-(1:burnIn),2]))  # draw a vertical line indicating the mean value of sampled b
  abline(v = trueB, col="red" )  # draw a red vertical line indicating the true value of b
  hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")   # draw the histogram of kept sampling of c
  abline(v = mean(chain[-(1:burnIn),3]) )  # draw a vertical line indicating the mean value of sampled c
  abline(v = trueSd, col="red" )  # draw a red vertical line indicating the true value of c
  plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a")  # draw the line plot of the kept sample of a
  abline(h = trueA, col="red" )  # draw a horizontal line indicating the true value of a
  plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b")  # draw the line plot of the kept sample of b
  abline(h = trueB, col="red" )  # draw a horizontal line indicating the true value of b
  plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd")  # draw the line plot of the kept sample of c
  abline(h = trueSd, col="red" )  # draw a horizontal line indicating the true value of c
}

# compare outcomes function

## note: I designed this function to take a vector of different iteration numbers, and return 
## a merged result. And of course you can input just one iteration number, and it will return
## the result for only one iteration number.

compare_outcomes <- function(iteration.numbers){
  num.of.iteration <- length(iteration.numbers)  # record the number of different iteration times
  loop.num <- 10
  
  record.compare.mean <- array(dim=c(loop.num, num.of.iteration))
  record.compare.std <- array(dim=c(loop.num, num.of.iteration))
  
  for (i in c(1:loop.num)){
    startvalue <- c(1:3)
    startvalue[1] <- runif(n=1, min=0, max=10)
    startvalue[2] <- rnorm(n=1, sd = 5)
    startvalue[3] <- runif(n=1, min=0, max=30)
    
    for (j in c(1:num.of.iteration)){
      chain = run_metropolis_MCMC(startvalue, iteration.numbers[j])
      a.mean <- mean(chain[, 1])
      record.compare.mean[i, j] <- a.mean
      a.std <- sd(chain[, 1])
      record.compare.std[i, j] <- a.std
    }
  }
  
  record.compare <- array(dim=c(loop.num, num.of.iteration))
  
  for (i in c(1:loop.num)){
    for (j in c(1:num.of.iteration)){
      record.compare[i, j] = paste("mean:", round(record.compare.mean[i, j], 3), 
                                   "std:", round(record.compare.std[i, j], 3))
    }
  }
  
  record.compare <- data.frame(record.compare)
  
  for (j in c(1:num.of.iteration)){
    colnames(record.compare)[j] <-  paste("iteration:", iteration.numbers[j])
  }
  
  for (j in c(1:num.of.iteration)){
    colnames(record.compare)[j] <-  paste("iteration:", iteration.numbers[j])
  }
  
  for (i in c(1:loop.num)){
    rownames(record.compare)[i] <- paste("loop ", i)
  }
  
  return(record.compare)
}