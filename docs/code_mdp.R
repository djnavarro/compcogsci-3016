
# This is probably the most complicated code that I've distributed. I've tried
# to organise it so that it's relatively easy to follow, but it's never going
# to be simple if only because MDPs are not the easiest thing in the world to
# understand. The two most important things to understand are the ITERATE 
# function (starts on line 112) and the BESTPOLICY function (line 131). These
# two together form the core of the value iteration algorithm. The rest of the
# code is more about the specific "toddler navigation" problem itself rather
# than MDPs in general.


# free parameters you can play with
discount <- .9  
fidelity <- .8



####### variables that need to be in the scope chain ####### 

# parameters
nx <- 8
ny <- 8

# grid size
ns <- nx*ny


############## agent-environment interaction ##############  

# the noisyMove function is what defines the agent's interactions with the environment. 
# for any given state (i.e. FROM) and any given intended ACTION, it returns either a 
# probability distribution over possible locations (sample=FALSE) or an actual location
# that the agent ends up with (sample=TRUE) that is sampled from this distribution. The
# FIDELITY with which the agent's actions match its intentions is also input.

noisyMove <- function(from, action, fidelity=.9, sample=FALSE){
  
  # staying is safe: no noise here
  if(action == "stay" ) { 
    
    event <- list("stay") # the event
    outcomes <- 1 # the probability that it occurs

    
  } else { # moving is noisy...
    
    # given the intended action, produce a list of possible events that might actually occur
    if(action=="up") { event <- list("up", "left", "right", c("up","left"), c("up","right")) }
    if(action=="down") { event <- list("down", "left", "right", c("down","left"), c("down","right") ) }
    if(action=="left") { event <- list("left", "up", "down", c("left","up"), c("left","down")) }
    if(action=="right") { event <- list("right", "up", "down", c("right","up"), c("right","down")) }
    
    # given a particular fidelity (i.e., goodness of motor control), compute the probabilities of each
    # of the possible events
    outcomes <- c(fidelity,rep.int((1-fidelity)/4,4))
    
  }
  
  # the moveTo takes a current state as input, plus an action, both 
  # specified by name, and then returns the location that the agent 
  # would end up in if motor control is perfect
  moveTo <- function(from, action) {
    t <- as.numeric(strsplit(from,",")[[1]]) # state name is in (y,x)
    if( action=="up") t[1] <- min( t[1]+1, ny )
    if( action=="down") t[1] <- max( t[1]-1, 1 )
    if( action=="left") t[2] <- max( t[2]-1, 1 )
    if( action=="right") t[2] <- min( t[2]+1, nx )
    t <- paste(t,collapse=",") # convert back to state name to string
    return(t)
  }
  
  # now, for each of the possibilities named in the event list, figure out where the agent would 
  # end up given their current state
  ne <- length(event)
  loc <- vector(length=ne)
  for( i in 1:ne ) { # across all possible events listed
    l <- from # start location
    for( e in event[[i]] ) { # apply each of the actions (e.g., just "up", or possibly "up" and "right")
      l <- moveTo(from, e) # update the location
    }
    loc[i] <- l # store the final location that the agent has ended up in
  }
  
  # if there are duplicates (multiple possibilities leading to the same location due to a wall), 
  # aggregate them so that you've got a unique set of final locations 
  tmp <- aggregate(outcomes ~ loc, FUN= sum) 
  loc <- tmp[[1]]
  outcomes <- tmp[[2]]
  ne <- length(loc)
  
  # should we sample an event stochastically, and return that?
  if( sample ) {
    if( ne==1 ) return( loc ) 
    e <- sample(ne,1,prob=outcomes)
    return( loc[e])
  }
  
  # if not, return a named vector that specifies the probabilities
  names(outcomes) <- loc
  return(outcomes)
  
}

############## the core of the value iteration algorithm ##############  

# Define the value iteration function. The input is a VALUE vector that specifies
# the current estimate of the utility of each state, the REWARDS vector that specifies
# the rewards to be received by being in this state, the TRANSITION array, and the 
# DISCOUNT rate. The output is a NEWVALUE vector that specifies an updated estimate of
# the utility of each state 

iterate <- function(value,rewards,transition,discount){
  
  # initialise
  ns <- length(value)
  newValue <- vector(length=ns)
  names(newValue) <- names(value)
  
  # loop over states 
  for( s in 1:ns ) {
    newValue[s] <- rewards[s] + discount * max( value %*% transition[s,,] ) # apply Bellman update
  }
  return(newValue)
  
}

# Convert a VALUE vector and a TRANSITION array into a decision POLICY: for each state, the 
# optimal policy is to choose the action that has highest expected utility given that you are
# in that state

bestPolicy <- function(value, transition) {
  
  # initialise
  ns <- dim(transition)[1]
  na <- dim(transition)[3]
  policy <- vector(length=ns)
  
  # loop over states
  for( s in 1:ns ) {
    policy[s] <- which.max( value %*% transition[s,,] ) # select action with highest expected value
  }
  
  # make the output pretty: each action is referred to by its name (not a number) and each element
  # of the policy vector is given a name (i.e., the name of the relevant state)
  actions <- dimnames(transition)[[3]]
  policy <- actions[policy]
  names(policy) <- dimnames(transition)[[1]]
  return(policy)
  
}


############## plotting function ##############  

plotMDP <- function( rewards, policy=NULL ) {
  
  # helper function that defines a colour map running from 
  # red to blue
  redToBlue <- function(n) {
    step <- 1/((n-1)/2)
    red <- seq(1,0,-step) 
    red <- c(rep.int(1,length(red)-1), red)
    blue <- red[seq(length(red),1)]
    green <- c(seq(0,1,step), seq(.9,0,-step))
    cmap <- rgb( red, green, blue )
    return(cmap)
  }
  
  # define the colour map 
  cmap <- redToBlue(n=201)
  
  # plot the rewards
  R <- matrix(rewards,ny,nx,TRUE)
  image(1:nx,1:ny,t(R), xlab="",ylab="", col=cmap,
        zlim=c(-20,20))
  
  # add the policy if specified
  if( !is.null(policy) ) {
    P <- matrix(policy,ny,nx,TRUE)
    pc <- c("stay"="o", "up"="^","down"="v","left"="<", "right"=">")
    for( i in 1:ny ) {
      for( j in 1:nx ) {
        text(x=j,y=i,labels=pc[P[i,j]])
      }
    }
  }
}



############## set up action list, reward vector and transition array ##############  

# specify the actions the agent can take
actions <- c("stay","up","down","left","right")
na <- length(actions)

# look up tables
stateLoc <- arrayInd(1:ns,.dim=c(nx,ny))[,2:1]
stateNum <- matrix(1:ns,ny,nx,TRUE)

# state names are strings that take the form of y,x co-ordinates: e.g. "6,3"
states <- apply(stateLoc,1,paste,collapse=",")  

# define the rewards in matrix form
rewardMat <- matrix(0,ny,nx) # note that co-ords are y,x
rewardMat[8,1] <- 5 # the chocolate
rewardMat[1,8] <- 2 # the cookie
rewardMat[2,3:6] <- -5 # sharp corners
rewardMat[6,3:5] <- -1 # pebbles
rewardMat[4,5] <- -1 # pebbles
rewardMat[6,1:2] <- -20 # the nails
  
# convert to vector
rewards <- rewardMat[stateLoc]

# which states are "terminal" (i.e., end the game)
terminalStates <- matrix(0,ny,nx) 
terminalStates[8,1] <- 1 # eating the chocolate ends the game
terminalStates[1,8] <- 1 # eating the cookie ends the game

# convert to vector
terminal <- terminalStates[stateLoc]

# initialise the transition array. transition[F,T,A] specifies the probability of ending up in 
# state T, given that you are currently in state F, and undertake action A
transition <- array( data=0, dim=c(ns,ns,na),
                     dimnames=list(
                       from = states,
                       to = states,
                       action = actions
                     ))


# populate the transition array
for( a in actions ) { # a is the name of an action (e.g., "up")
  for( f in states ) { # f is the name of a state 
    pt <- noisyMove(from=f,action=a,fidelity=fidelity) # probability distribution over locations
    transition[f,names(pt),a] <- pt # store it
  }
}

# now enforce terminal states
ts <- states[terminal==1]
transition[ts,,] <- 0 # there are no future actions once you hit a terminal state


# clean up
rm( stateLoc, stateNum, rewardMat, terminalStates, ts, terminal)




############## run the MDP ##############  

# initial values for each state correspond to the immediate rewards
value <- rewards

# set up 
eps <- .01
diff <- 1
it <- 0

while( diff > eps ) { # continue until changes between current and previous values are negligible
  
  # display
  it <- it+1
  if( it %% 50 == 0 ) cat(".")
  
  # iteration
  oldValue <- value
  value <- iterate(value, rewards, transition, discount)
  diff <- max( abs(value - oldValue ))
  
}

# results
policy <- bestPolicy(value,transition) # extract the optimal policy from the values
plotMDP( rewards, policy ) # plot the policy



