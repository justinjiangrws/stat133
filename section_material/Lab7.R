# Lab 7: Simulation Example

#7.1 Using while loop to control maximum iterations:
#Silly example here: draw one number from 1 to 100 and see if this number matches with our predefined value.
hit <- function(target){
  a <- sample(1:100, size=1)
  return(list(a, a == target))
}

hit(5)

sim_hit <- function(target, max.it=200){
  i <- 1 #the counter
  values <- c() #dummy vector to store results
  
  while (i < max.it){
    res <- hit(target)
    values[i] <- res[[1]]
    if (res[[2]]){break}
    i <- i+1
  }
  return(list(values, i, res[[2]]))
}

results <- sim_hit(5)
results

#We can change max. iterations to save time (not in this case beacause this simulation is very simple and fast):
sim_hit(5, max.it=100)

#7.2 Exploring different values
targets <- seq(10, 100, by=10)

B <- 1000
result.matrix <- matrix(nrow=B, ncol=length(targets))
for (i in 1:length(targets)){
  #print(i)
  result.matrix[,i] <- replicate(B, sim_hit(target=targets[i])[[2]])
}

colnames(result.matrix) <- targets

#7.3 Example Visualization Using Boxplot (side-by-side):
boxplot(result.matrix, xlab="Target Value", ylab="#Iterations")

#All distributions look similar, just as expected. 
apply(result.matrix, 2, mean)
