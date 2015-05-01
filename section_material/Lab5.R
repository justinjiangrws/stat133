#Lab 5 Functions, Loops and Bootstrap
#5.1 Functions
#5.1.1 Anatomy of functions in R
#The structure of a function is given below.

#functionName <- function(arg1, arg2, ... ){
#  function body
#  return(object)
#}

#5.1.2 Examples

#Here is an example of user defined function. This function will calculate
#a + |b|, a plus the absolute value of b.Function is stored in the name 
#aPlusAbsb. It takes two arguments, a and b.

#Example 5.1

aPlusAbsb <- function(a, b){
  if(b >= 0) {
    return (a + b)
  } else {
    return (a - b)
  }
}

aPlusAbsb(1, -2)

aPlusAbsb(1, 2)

#If there is no return statements in the function, it only REPORTS the last 
#of the computations.

#Compare the following two functions:
  
#  Example 5.2

func.bad <- function(x, y) {
  z1 <- 2*x + y
  z2 <- x + 2*y
  z3 <- 2*x + 2*y
  z4 <- x/y
}

func.good <- function(x, y) {
  z1 <- 2*x + y
  z2 <- x + 2*y
  z3 <- 2*x + 2*y
  z4 <- x/y
  return(c(z1, z2, z3, z4))
}

result.good <- func.good(-1, 1)
result.bad <- func.bad(-1, 1)
result.good
result.bad

#2. Loops
#2.1 While loop
#The while loop is used when you want to keep iterating as long as a 
#specific condition is satisfied. The basic structure of the while loop:
  
#  while(condition){ commands }

#Example of a function printing integers from 1 to n.

#Example 5.3

printOneToN <- function(n) {
  i <- 1 
  while(i <= n) {
    print(i)
    i <- i+1
  }
}

printOneToN(4)

#2.2 For loop
#The for loop is used when iterating through a vector. The basic structure 
#of the for loop:
  
#  for(index in vector){ commands }

#Now let's write example 5.3 with a for loop.

#Example 5.5

printForOneToN <- function(n) {
for(i in 1:n) {
print(i)
}
}

printForOneToN(3)


#3 Implement bootstrap
#Bootstrapping can be a very useful tool in statistics and it is very
#easily implemented in R. Bootstrapping is a nonparametric method which
#lets us compute estimated standard errors, confidence intervals 
#and hypothesis testing.

#Example 5.6
# We will implement the bootstrap estimator of the variance of the mean 
# We analyse the sepal length in the iris dataset.

#The basic idea is to permute the data by sampling with replacement:
n <- 100
boot_mean <- rep(0,100)
for (i in 1:n){
  dat <- sample(x=iris$Sepal.Length, size=nrow(iris), replace=T)
  boot_mean[i] <- mean(dat)
}
var(boot_mean)


#Or equivalently, using replicate:
n <- 100
boot_data <- replicate(n,{iris$Sepal.Length[sample(nrow(iris), replace = TRUE)]})
boot_mean <- colMeans(boot_data)
var(boot_mean)
