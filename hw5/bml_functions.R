#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  if (p > 1 | p < 0){
    stop ("Invalid Probability (p must be between 0 and 1.)")
  }
  samp <- sample(0:2, size = r*c, replace = TRUE, prob = c(1-p,p/2,p/2))
  m <- matrix(data = samp,nrow = r, ncol = c)
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  m_new <- m
  grid.new <- TRUE
  for (i in 1:nrow(m)){ #forbasecase
#     can_move_changes <- NULL
    for (j in 1:ncol(m)){
#      print (paste0("column",j))
      can_move <- FALSE
      if (m[i,j] == 1){
        if (j == ncol(m)){
          if (m[i,1] == 0){
            m_new[i,j] <- 0
            m_new[i,1] <- 1
#            print (m_new)
          }
        }else{
          if (m[i,j+1] == 0){
            m_new[i,j] <- 0
            m_new[i,j+1] <- 1
#            print (m_new)
          }
        }  
      }
#       print(can_move)
#       can_move_changes <- c(can_move_changes,can_move)
#       print(can_move_changes)
    }
#     for (k in 1:length(can_move_changes)){
#       if (can_move_changes[k] == TRUE){
#         m[i,j] <- 0
#         if (j == ncol(m)){
#           m[i,1] <- 1
#         }else{
#           m[i,j + 1] <- 1
#         }
#       } 
#     }
  }
  m_new2 <- m_new
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
#       can_move <- FALSE
      if (m[i,j] == 2){
        if (i == 1){
          if (m_new[nrow(m),j] == 0){
            m_new2[i,j] <- 0
            m_new2[nrow(m),j] <- 2
          }
        }else{
          if (m_new[i-1,j] == 0){
            m_new2[i,j] <- 0
            m_new2[i-1,j] <- 2
          }
        }  
      }
    }
  }
  if (all(m == m_new2)){
    grid.new <- FALSE
  }
  return(list(m_new2, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p, step = 1000){
  mat <- bml.init(r,c,p)
  gridlock <- FALSE
  print (mat)
  m1 <- bml.step(mat)
  steps_taken <- 1
  print (m1)
  while (gridlock == FALSE & steps_taken < step){
    if (m1[[2]] == FALSE){
      gridlock <- TRUE
    }
    m1 <-bml.step(m1[[1]])
    print (m1)
    steps_taken <- steps_taken + 1
    print (steps_taken)
  }
 return (steps_taken)
}
