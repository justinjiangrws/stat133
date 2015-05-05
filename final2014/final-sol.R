#################################################################
# Statistics 133, Lecture 1, Fall 2014
# Final Exam, Friday December 19th, 7 pm - 10 pm

# Please read all instructions carefully.
#################################################################
# IMPORTANT
# Enter your personal information here, between the quotation marks:
name <- ""
github.name <- ""
email.address <- ""

#################################################################
# The exam has a total of 100 points and for each task 
# the point is given in square brackets.
# The exam has 6 parts and the total number of points by part is:
# 15 pts : Part I, general R commands
# 20 pts : Part II, plotting
# 15 pts : Part III, apply and by statements
# 20 pts : Part IV, functions 
# 10 pts : Part V, simulations
# 20 pts : Part VI, string manipulation and regular expressions
#100 pts in Total

# If you spend about 30 min or so reading instructions and the like
# this gives you 1.5 min per point -- the exam is LONG ON PURPOSE!
# Please just work your way through it the best you can, prioritizing
# things you can easily do and remember everyone else is also
# racing the clock and will be graded on a curve.

#################################################################
# DO NOT manually set the path anywhere in this script, instead:

# RIGHT NOW!!   In the RStudio menu select:
# Session > Set Working Directory > To Source File Location

# NOTE!
# All "load" statements are included in the file already,
# DO NOT change them in any way or add a path to them,
# it will prevent us from being able to run your script.

# NOTE!
# There are a few calls to the function set.seed()
# DO NOT change these in any way.

#################################################################

#### PART I : General R commands [15 pts]

# [1 pt]
# Create [x], a numeric vector of length 1000 with 
# entries: 5, 10, 15, etc.
#x <- <your code here>
x <- 5*(1:1000)
#x <- seq(from=5, to=5000, by=5)
#x <- seq(from=5, by=5, length.out=1000)
  
# [1 pt]
# Create [y], a logical vector of length 1000 
# with y[i]=T if x[i] is divisible by 10, otherwise F

 # y <- <your code here>
  
 y <- ifelse(x%%10, F, T)
  
# [1 pt]
# Create [z], a numeric vector of length 111 with entries
# that are drawn from a standard normal distribution (hint: rnorm)
# *and* stored in increasing order
set.seed(42)
#z <- <your code here>
  
 z <- sort(rnorm(111))
  
# [1 pt]
# Create [v], a numeric vector with :
# a random permutation of the even numbers from 2 to 222
  set.seed(31415)
#v <- <your code here>
 v <- sample(2*(2:111))
  
# [1 pt]
# Create [w], a random permutation of the numeric values of a deck of cards
# (i.e. just the numbers 1 through 13 each repeated 4 times)
set.seed(2718)
#w <- <your code here>
 w <- sample(rep(1:13, each=4))
  
# [1 pt]
# Create [m], a matrix of size 10x10 with entries that are 
# Exponential random variables (hint: rexp) with rate 3
# (arrange the values by column, as per default)
set.seed(344)
#m <- <your code here>
 m <- matrix(rexp(100, rate=3), ncol=10)
  
# [1 pt]
# Create [l], a list with 12 elements, each a vector of length 100.
# Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
  set.seed(71)
#<your code here>
l <- list(); for(i in 1:12) l[[i]] <- rpois(n=100, lambda=5)

# For the next few tasks you will use the data frame family (size 14x5)
# LEAVE AS IS:
load("family.rda")

# [1 pt]
# Create [f1] a subset of family with only women over age 50
#f <- <your code here>
 f <- family[ family$gender=='f' & family$age>50 , ]
  
# [1 pt]
# Create [f2] a subset of family with only men 6 foot tall or more
#fm <- <your code here>
fm <- family[ family$gender=='m' & family$height >= 72 , ]
  
# [1 pt]
# Create [f3] a subset of family of people whose name starts with T
#f3 <- <your code here>
  
 f3 <- family[ substr(family$name, 1, 1)=="T" , ]

# [1 pt]
# Create [f4] a subset of family with just the youngest individual (so just one row)
#f4 <- <your code here>
f4 <- family[ family$age==min(family$age), ]

# for the next two tasks you will use the data frame infants (size 1236x15)
# LEAVE AS IS:
load("KaiserBabies.rda") 

# [2 pt]
# Create a table [t] of the education level ($ed) of all married ($marital) first time ($parity=1) mothers:
#t <- <your code here>

t <- table(infants$ed[ infants$married=="Married" & infants$parity==1])

# [2 pt]
# Calculate [mw], the average birthweight ($bwt) of all babies whose were full term, i.e. gestation equal or more than 259 days.
#mw <- <your code here>
  
 mw <- mean(infants$bwt[infants$gestation>=259])
  
#################################################################
#### PART II : Plotting [20 pts]

##### Flowers [8 pts total, 2+3+3]
# We will now use the dataset "iris" which is icluded in the R package.
# To look at the dataframe you can just type "iris" at the prompt
# It is a data frame of size 150x5 with measurements of 4 attributes
# for 150 flowers, 50 each of 3 different species of irises.

# [2 pts]
# Make a box plot of Sepal Length by Species (so 3 boxplots in one plot)

 boxplot(iris$Sepal.Length ~ iris$Species)

# [3 pts]
# Make a scatterplot of petal width (y-axis) versus petal length (x-axis)
# The axes labels should be "Petal Length" and "Petal Width",
# Color the plotting symbol by Species (any 3 colors)

 plot(iris$Petal.Length, iris$Petal.Width, xlab="Petal Length", ylab="Petal Width", col=as.numeric(iris$Species))

# [3 pt]
# Make a scatterplot of ( sepal length / petal length) as a function of index (order)
# Color the plotting symbol by Species (any 3 colors)

 plot(iris$"Sepal.Length"/iris$"Petal.Length", col=as.numeric(iris$Species))

##### We will now use the infant birth data again (data frame infants)

# [6 pts]
# Make a scatterplot of infant birthweight in ounces (bwt) vs. gestation time in days (gestation)
# The plotting symbol should be a red star (*)
# Put on custom made x-axis and y-axis labels that fully describe the variables
# Add a vertical line at gestation=259 (full length pregnancy)

plot(infants$bwt ~ infants$gestation, pch='*', col="red", xlab="Gestation in days", ylab="Birth weight in oz")
abline(v=259)

# [6 pts]
# Make a histogram of mother's age (age) and superimpose on it a _blue_ density plot (same variable)
# Note that the y-axis of the histogram and the density have to be the same...
# Add x-axis labels
hist(infants$age, prob=T, xlab="Mother's age")
lines(density(infants$age, na.rm=T), col="blue")

#################################################################
##### PART III : apply and by statements [15 pts]

# For the next few tasks you will use the list Cache500 
# (list of length 500, each element is a numeric vector of various lengths)
# LEAVE AS IS:
load("Cache500.rda")

# [3 pts]
# Create [first.cache], a vector where each entry is the _first_ element of the
# corresponding vector in the list Cache500

#first.cache <- <your code here>
first.cache <- sapply(Cache500, "[", 1)

# [3 pts]
# Create [mean.cache], a vector of length 500 where each entry is the mean 
# of the corresponding element of the list Cache500

#mean.cache <- <your code here>

mean.cache <-  sapply(Cache500, mean)

# [2 pts]
# Create [sd.cache], a vector of length 500 where each entry is the sd
# of the corresponding element of the list Cache500

#sd.cache <- <your code here>
  
sd.cache <-  sapply(Cache500, sd)


# [4 pts]
# Create [mean.long.cache], a vector where 
# mean.long.cache[i] is:
# the mean of Cache500[[i]] IF it has 50 or more entries.
# NA IF Cache500[[i]] has less than 50 entries.

#mean.long.cache <- <your code here>
mean.long.cache <-  sapply(Cache500, function(x){ if(length(x)>=50) return(mean(x)) else return(NA)})

# Consider again the iris dataset
# [3 pts]
# Create a variable [max.petal.width] _a numeric vector of length 3_
# that has the maximum petal length for each iris species.
max.petal.width <- as.vector(by(iris$Petal.Width, iris$Species, max))

#################################################################
##### PART IV : functions [20 pts]

# [6 pts]
# Write a function [firstColToNames] that takes a matrix or a data frame
# and converts the first column to row names.
# Input:  a matrix or a data frame
# Output : a matrix of data frame that is like the input except 
# -- the first column has been removed
# -- what was the first column is now row names.

# Example, make a small 2x4 matrix, test:
# test <- matrix(c(1, 5, 3, 8, 2, 5, 7, 9), ncol=4, byrow=T)
# > test
# [,1] [,2] [,3] [,4]
# [1,]    1    5    3    8
# [2,]    2    5    7    9
# The output from firstColToNames(test) should be a 2x3 matrix with row names
# > firstColToNames(test)
#   [,1] [,2] [,3]
# 1    5    3    8
# 2    5    7    9

## firstColToNames <- function(  ){
##   <your code here>
  
## }

firstColToNames <- function(m){
  # students do not need to include this test
  if(length(dim(m))!=2) print("m is not a matrix or dataframe")
  else{
     names <- as.character(m[,1])
     m <- m[, -1]
     rownames(m) <- names
     return(m)
  }
}
    
# [6 pts]
# Write a function [longerRange()] with
# Input [m1 and m2] : two numeric matrices or data frames, don't have to have the same dimension 
# Output : 1 if the range of m1 is larger than or equal to the range of m2, 
#        : 2 otherwise 
# The range is the distance between the maximum and minimum values
# The function should ignore NA values (i.e. if a matrix has an entry that is NA)

## longerRange <- function(m1, m2){
##   <your code here>  
## }

longerRange <- function(m1, m2){
  tmp1 <- max(m1, na.rm=T)-min(m1, na.rm=T)
  tmp2 <- max(m2, na.rm=T)-min(m2, na.rm=T)
  if(tmp1 >= tmp2) return(1)
  else return(2)
}

# [8 pts]
# Write a function [TempConv()] that takes
# Inputs
# [t] : a temperature (numeric value)
# [scale] : a character "F" or "C" depending on whether the 
#           input temperature is in Fahrenheit or Celcius
# Output 
# [t.new] : the temperature converted to the other scale.

# The conversion formulas are: 
# F = C * 9/5 + 32
# C = (F - 32) * 5/9
# so e.g. 30 F=-1.11 C and 30 C = 86 F

## TempConv <- function(t, scale){
##   <your code here>
  
## }

TempConv <- function(t, scale){
  if(scale=="F") return((t - 32) * 5/9)
  else if(scale=="C") return(t * 9/5 + 32)
}
#################################################################
##### PART V : simulations [10 pts]

# leave this here:
set.seed(123456)

# [5 pts]
# Write a function, [dice_sum()], that takes as input:
# [k] : the number of dice rolled
# [B] : the number of rolls
# and returns:
# [dsum] : a vector of length B where each element is the sum of a roll 
#          k dice 

# So if k=1 pick a number between 1 and 6 at random B times and return them,
# if k=2 then in each roll you pick twice a number between 1 and 6 at random,
# calculate their sum, do this B times and return
# and so on.

dice_sum <- function(k=2, B=100){
   replicate(B, sum(sample(1:6, size=k, replace=T)) )
}

# [5 pts]
# Lets run four simulations:
# Fix k=2 for all simulations
# Use B=20, 100, 1000, 5000 (in this order)

# For each value of B we will:

# Calculate the mean and sd of the output and store in 
# [ave.diceRoll] : a vector of length 4 where each entry is the mean of the simulation output
# [sd.diceRoll] : a vector of length 4 where each entry is the sd of the simulation output

# Then plot four histograms of the output from dice_sum()
# On each histogram the x-axis label should be "sum of dice roll" and the title should be
# "Histogram for B=[correct number]"

# CAREFUL : just run 4 simulations and save the mean and sd each time and make the histogram
# DO NOT : run 4 simulations to get the mean, then another 4 simulations to make the plots.

# NOTE : if for some reason you can not write the function [dice_sum] create four vectors
# of length 20, 100, 1000 and 5000 and complete the tasks below.

#----------------------
# To get all four histograms on one plot we include a command that splits the plotting
# screen into four panels, 2 in each row.  When a plotting function is called the plot
# goes into the first panel (upper left), when you call a plotting function again it goes
# into the second panel (upper right), etc.

# DO NOT DELETE THIS:
par(mfrow=c(2,2))
#----------------------

ave.diceRoll <- rep(0, 4)
sd.diceRoll <- rep(0,4)
Bvec <- c(20, 100, 1000, 5000)

for(i in 1:4){
   tmp <- dice_sum(k=2, B=Bvec[i])
   ave.diceRoll[i] <- mean(tmp)
   sd.diceRoll[i] <- sd(tmp)
   hist(tmp, xlab="sum of dice roll", main=paste("Histogram for B=", Bvec[i]))
}

# STOP: did you remember to put in the title and axis label on the histograms?

#################################################################
##### PART VI : string manipulation and regular expressions [20 pts]

phrases <- c("coat", "cat", "ct", "mat", "Sat!", "Now?", "match", "How much? $10", "7 cats", "ratatatcat", "atatatatatatatatat")

# [2 pts]
# Create a vector [text1] that lists the elements in phrases that have 
# a match to "at", anywhere 
#text1 <- <your code here>
text1 <- grep("at", phrases)

# [2 pts]
# Create a vector [text2] that lists the elements in phrases that have 
# a match to "at", _at the end of the phrase_ 
#text2 <- <your code here>
text2 <- grep("at\\>", phrases)

# [4 pts]
# Create a vector [text3] that lists the elements in phrases that have 
# a match to any multiple of "at", _two or more times_ (atat" or "atatat" etc.)
# and anything before or after that match
#text3 <- <your code here>
text3 <- grep(".+((at){2,}).+", phrases)

# [3 pts]
# Create a vector [tests] that is of length 200 and has the entries
# "test1", "test2", ..., "test200"

#tests <- <your code here>
  tests <- paste("test", 1:200, sep='')

# [3 pts]
# Take the vector [tests] from above and create a character string
# [tests.all] (so a vector of length 1)
# that stores the entries of [tests] as one long string
# i.e. tests.all should be "test1 test2 test3 ... test200"
#tests.all <- <your code here>

tests.all <- paste(tests, collapse=" ")

# [6 pts]
# Start with [minchin] which is a character string, create 
# a _vector_ (not list) [minchin.split] which 
# stores the words of [minchin] each as a separate element.
# Also, convert all upper case letters to lower case.
# You can leave punctuation marks in.

minchin <- "And try as hard as I like, A small crack appears In my diplomacy-dike. By definition, I begin, Alternative Medicine, I continue Has either not been proved to work, Or been proved not to work. You know what they call alternative medicine That's been proved to work? Medicine."

#minchin.split <- <your code here>
  
minchin.split <- tolower(unlist(strsplit(minchin, " ")))

  
#################################################################

