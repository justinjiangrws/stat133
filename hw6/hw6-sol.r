# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable has.adopted.
  # We define it as a matrix with all 0 then use initial.doctors
  # to set the first column (first day)
  has.adopted <- matrix(0, nrow=n.doctors, ncol=n.days)
  has.adopted[,1] <- initial.doctors
  
  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  for(i in 2:n.days){
     # initialize the day to be the same as yesterday:
     has.adopted[,i] <- has.adopted[,i-1]
     # pick two random doctors
     two.doctors <- sample(1:n.doctors, size=2, replace=FALSE)

     # check if one has adopted and the other one hasn't
     # i.e. whether one is 0 and one is 1, or equivalently, their sum is 1
     # we can use either condition in the if-statment:

     # OPTION1: has.adopted[two.doctors[1], i] != has.adopted[two.doctors[2], i]
     # OPTION2: sum(has.adopted[two.doctors, i]) == 1
     
     
     if(sum(has.adopted[two.doctors, i]) == 1){
         # with probability p we change the non-adopted to an adopter
         # we can generate a Bernoullie 0/1 varilable or use a Uniform(0,1) variable:

         # ONE OPTION: if(rbinom(n=1, size=1, prob=p) == 1){
         if(runif(1) < p){
            # set both of the doctors to 1 (saves us checking who was 0 and who was 1)
            has.adopted[two.doctors, i] <- 1
        }
     }
  }
  
  # return the output
  return(has.adopted)
}


# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

