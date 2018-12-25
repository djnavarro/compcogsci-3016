animalGeneralisation<- function( premises, phi=20, plot=TRUE ) {
  
  # PREMISES should be a vector of item names. (required)
  # PHI is a number that describes the strength of the 
  #   simplicity bias in the prior. (default=20)
  # PLOT is a logical that indicates whether to draw a 
  #   picture of the generalisations (default=TRUE)
  
  # names of the animals
  items <- c("horse", "cow", "elephant", "rhino", "chimp", 
               "gorilla", "mouse", "squirrel","dolphin", "seal" )
  
  # The "base" representation is a simple binary tree structure.
  # In the original work, Sanjana & Tenenbaum derived this tree
  # by applying a hierarchical clustering algorithm to human 
  # similarity judgments. 
  tree <- rbind(
    
    c(1,0,0,0,0,0,0,0,0,0), # all the singleton clusters
    c(0,1,0,0,0,0,0,0,0,0),
    c(0,0,1,0,0,0,0,0,0,0),
    c(0,0,0,1,0,0,0,0,0,0),
    c(0,0,0,0,1,0,0,0,0,0),
    c(0,0,0,0,0,1,0,0,0,0),
    c(0,0,0,0,0,0,1,0,0,0),
    c(0,0,0,0,0,0,0,1,0,0),
    c(0,0,0,0,0,0,0,0,1,0),
    c(0,0,0,0,0,0,0,0,0,1),
    
    c(1,1,0,0,0,0,0,0,0,0), # all the pairs in the tree
    c(0,0,1,1,0,0,0,0,0,0),
    c(0,0,0,0,1,1,0,0,0,0),
    c(0,0,0,0,0,0,1,1,0,0),
    c(0,0,0,0,0,0,0,0,1,1),
    
    c(1,1,1,1,0,0,0,0,0,0), # the bigger ones
    c(1,1,1,1,1,1,0,0,0,0),
    c(1,1,1,1,1,1,1,1,0,0),
    c(1,1,1,1,1,1,1,1,1,1)
    
  )
  colnames(tree) <- items # attach nice labels
  
  # useful numbers
  nClusters <- dim(tree)[1] 
  nItems <- length(items)
  nHypotheses <- sum( choose( nClusters, 1:3 )) # <- upper bound!
  
  
  # initialise a hypothesis space that consists of all possible
  # clusters, pairs of clusters, and triples of clusters. also
  # a belief vector that describes our prior over these clusters
  hypotheses <- matrix( 0, nrow=nHypotheses, ncol=nItems )
  colnames( hypotheses ) <- items
  belief <- vector( length=nHypotheses )
  
  # the first order hypotheses are just the 19 clusters defined 
  # by tree structure itself
  hypotheses[ 1:nClusters, ] <- tree
  belief[ 1:nClusters ] <- 1/phi
  
  # the second order hypotheses are the 171 unique pairs of 
  # clusters in the tree (or, as it will turn out, a subset
  # of these pairs)
  ind <- nClusters
  for( a in 1:(nClusters-1) ) {
    for( b in (a+1):nClusters ) {
      
      ind <- ind+1
      hypotheses[ind, ] <- tree[a,] | tree[b,] 
      belief[ind] <- (1/phi)^2
      
    }
  }
  
  # the third order hypotheses are the 969 unique triples of 
  # clusters in the tree (or, as it will turn out, a subset
  # of these pairs)
  for( a in 1:(nClusters-2) ) {
    for( b in (a+1):(nClusters-1) ) {
      for( c in (b+1):nClusters ) {
        
        ind <- ind+1
        hypotheses[ind, ] <- tree[a,] | tree[b,] | tree[c,]
        belief[ind] <- (1/phi)^3
              
      }  
    }
  }
  
  # now, we've been sloppy up to this point: many of these 
  # consequential sets are identical. for instance, there's 
  # a cluster for "horse" and a cluster for "cow" in the tree
  # and there's also a cluster for "horse,cow". so there's no
  # reason to include "horse"+"cow" as a composite hypothesis,
  # because it's already there as a first order one. 
  #
  # to fix this, we'll remove all duplicated rows from the 
  # hypothesis space. specifically, because the hypotheses
  # are ordered (first order at the top, third order at the
  # bottom), what we want to do is keep the *FIRST* instance
  # of a particular row. 
  #
  # the R function duplicated is perfect for this. It picks 
  # out all of the rows that are duplicates of rows above it
  redundant <- duplicated( hypotheses, MARGIN=1 )
  
  # keep only the non-redundant hypotheses, and then normalise
  # the belief vector so that it sums to 1
  hypotheses <- hypotheses[!redundant,]
  belief <- belief[!redundant]
  belief <- belief / sum( belief )
  nHypotheses <- length( belief ) # <- actual number of hypotheses
  
  # create likelihoods
  likelihood <- matrix( 0, nrow=nHypotheses, ncol=nItems)
  colnames(likelihood) <- items
  for( ind in 1:nHypotheses ) {
    likelihood[ind, hypotheses[ind,]==1 ] <- 1/sum(hypotheses[ind,])  
  }
  
  # now show the model the data, and sequentially update beliefs
  for( x in premises ) {
    belief <- belief * likelihood[,x]
  }
  belief <- belief / sum(belief) # must sum to 1
  
  # now compute the generalisation probabilities. I could do this
  # with loops, but it actually corresponds to a really simple
  # matrix multiplication: multiply the belief vector by the 
  # hypothesis matrix...
  generalisations <- belief %*% hypotheses
  
  # draw a picture if requested
  if( plot==TRUE ) {
    barplot( generalisations, ylab="generalisation probability", las=2,
             main=paste( "premises:", paste(premises,collapse=",")),
             font.main=1)
  }
  
  # return the generalisation vector, I suppose
  return( generalisations )
}



