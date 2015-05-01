###Function Definition
#Basic Syntax:
func = function(arg1, arg2=1){ #Give a default value to arg2
  return(arg1+arg2)
}

func(5)

##Arguments and return are not necessary:
f1 = function(){print("Hello World")}
f1()

##Example 1: Write a function that makes a histogram of the inputting data, 
##with a red vertical line showing its mean.
HistMean = function(dat){
  hist(dat)
  abline(v=mean(dat), col="red", lwd=5)
}

HistMean(rnorm(1000))

##Example 2:Write a function that simulate B values following a standard normal distribution,
##Discard the negative values.
sim = function(B){
  pool = rnorm(B)
  return(pool[pool > 0])
}

sim(100)
