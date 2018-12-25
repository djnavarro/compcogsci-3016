
bernoulliWalk <- function( p = .55, threshold=8, plot=TRUE  ) {
  
  # define the information function (i.e., the probability
  # distribution from which the agent receives evidence)
  informationFunction <- function( p ) {
    u <- runif(1) # random number between 0 and 1
    if( u < p ) {
      y <- 1  # evidence is +1 with probability p
    }  else {
      y <- -1 # evidence is -1 with probability 1-p
    }
    return(y)
  }
  
  # initialise variables
  x <- 0  # evidence accrued so far
  t <- 0  # time elapsed
  
  # now run the random walk...
  while( abs(x[t+1]) < threshold ) { 
    
    t <- t+1 # move forward one time point
    y <- informationFunction( p ) # collect one sample of evidence
    x[t+1] <- x[t] + y # increment the evidence tally
    
  }
  
  # output
  if( plot ) { plotWalk(x) } # draw picture as a side effect, if requested
  return( invisible(x) ) # return the evidence path but don't print it
  
}


# function to plot the decision process
plotWalk <- function(x, tmax=100, yscale=NULL){

  fpt <- length(x)-1 # read off the first passage time
  thresh <- abs(x[fpt+1]) # read off the decision threshold
  if( is.null(yscale) ) yscale <- thresh
  
  # create the plotting window
  plot.new()
  plot.window( xlim=c(0,tmax), ylim=yscale*c(-1,1) )
  
  # add y-axis
  axis(side=2, at=c(-thresh,0,thresh))
  title(ylab="Evidence")
  
  # add x-axis
  axis(side=1, pos=-yscale-2)
  title(xlab="Time")
  
  # plot the horizontal lines showing decision thresholds
  abline( h=-thresh )
  abline( h=thresh )
  
  # add the plot showing the evidence tally
  plotColour <- ifelse( x[fpt]>0, "blue","red" )
  lines( 0:fpt, x, lwd=2, col=plotColour )
  
}




