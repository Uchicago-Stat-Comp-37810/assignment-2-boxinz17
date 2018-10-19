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