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

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)
  output.doctors <- matrix(data = initial.doctors, nrow = n.doctors, ncol = n.days)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  for (i in 2:ncol(output.doctors)){
    sample.column <- sample(x = c(1:ncol(output.doctors)), size = 2, replace = FALSE)
    sample.doctors <- output.doctors[,i][sample.column]
    if (sample.doctors[1] == 1 | sample.doctors[2] == 1){
      if (runif(1) <= p){
        output.doctors[sample.column,i:ncol(output.doctors)] <- 1
      }
    }
  }
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
  return (output.doctors)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

simulation <- matrix(ncol = 5, nrow = 100)

for (i in 1:5){
  p <- 2*i
  sim.mat <- sim.doctors(sample(x = c(0,1), size = 200, prob = c(.9,.1), replace = TRUE), n.doctors = 200, n.days = 100, p)
  line <- colSums(sim.mat)
  simulation[,i] <- line
}

plot(simulation[,1], type = "l", xlab = "days", ylab = "Number of doctors adopting drug", main = "Drug Adoptions by Day", col = "blue", ylim = c(0,75))
lines(simulation[,2], col = "red")
lines(simulation[,3], col = "green")
lines(simulation[,4], col = "purple")
lines(simulation[,5], col = "brown")

legend("bottomright", fill=c("blue","red","green","purple","brown"), legend=paste0("p",seq(0.2,1,by=.2)),cex = 0.6)
