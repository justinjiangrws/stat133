## Stat 133 Midterm 2
## Thursday April 2nd

## General R commands

present <- "yes"

# [1 pt]
# Create [x], a numeric vector of length 1000 with 
# entries: 6, 12, 18, etc.

x <- seq(6,6000, by=6)


# [1 pt]
# Create [y], a logical vector of length 2000 
# with y[i]=T if x[i] is divisible by 10, otherwise F

y <- seq(1:2000)%%10 == 0

# [1 pt]
# Create [w], a random permutation of the numeric values of a deck of cards
# (i.e. just the numbers 1 through 13 each repeated 4 times)
set.seed(2718)
w <- sample(rep(1:13, each = 4), 52, replace = FALSE)


# [1 pt]
# Create [m], a matrix of size 10x10 with entries that are 
# Exponential random variables (hint: rexp) with rate 3
# (arrange the values by column, as per default)
set.seed(344)

m <- matrix(data = rexp(100, rate = 3), nrow = 10, ncol = 10)


# [1 pt]
# Create [l], a list with 12 elements, each a vector of length 100.
# Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
set.seed(71)
l <- list(rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5),rpois(100,lambda = 5))



# for the next two tasks you will use the data frame infants (size 1236x15)
# LEAVE AS IS:
load("KaiserBabies.rda") 

# [2 pt]
# Create a table [t] of the education level ($ed) of all married ($marital) first time ($parity=1) mothers:
t <- table(infants[infants$marital == 'Married' & infants$parity == 1,c("ed")])


# [2 pt]
# Calculate [mw], the average birthweight ($bwt) of all babies whose were full term, i.e. gestation equal or more than 259 days.
mw <- mean(infants[infants$gestation >= 259, c("bwt")], na.rm = TRUE)


# For the next few tasks you will use the data frame family (size 14x5)
# LEAVE AS IS:
load("family.rda")

# [1 pt]
# Create [f1] a subset of family with only women over age 50
f1 <- family[family$gender == 'f'& family$age >50,]

  
# [1 pt]
# Create [f2] a subset of family with only men 6 foot tall or more
f2 <- family[family$gender == 'm'& family$height >=72,]

  
# [1 pt]
# Create [f3] a subset of family of people whose name starts with T
f3 <- family[family$name == "T",]
  


# [1 pt]
# Create [f4] a subset of family with just the youngest individual (so just one row)
f4 <- family[family$age == min(family$age),]




## Plotting

# We will now use the dataset "iris" which is icluded in the R package.
# To look at the dataframe you can just type "iris" at the prompt
# It is a data frame of size 150x5 with measurements of 4 attributes
# for 150 flowers, 50 each of 3 different species of irises.

# [2 pts]
# Make a box plot of Sepal Length by Species (so 3 boxplots in one plot)
boxplot(iris[iris$Species =='setosa',c("Sepal.Length")], iris[iris$Species =='versicolor',c("Sepal.Length")], iris[iris$Species =='virginica',c("Sepal.Length")])

# [3 pts]
# Make a scatterplot of petal width (y-axis) versus petal length (x-axis)
# The axes labels should be "Petal Length" and "Petal Width",
# Color the plotting symbol by Species (any 3 colors)
plot(x = iris[iris$Species =='setosa',c("Petal.Length")], y = iris[iris$Species =='setosa',c("Petal.Width")], xlab = "Petal Length", ylab = "Petal Width", col ="red")
plot(x = iris[iris$Species =='versicolor',c("Petal.Length")], y = iris[iris$Species =='versicolor',c("Petal.Width")], col ="blue")
plot(x = iris[iris$Species =='virginica',c("Petal.Length")], y = iris[iris$Species =='virginica',c("Petal.Width")], col ="green")

# [3 pt]
# Make a scatterplot of ( sepal length / petal length) as a function of index (order)
# Color the plotting symbol by Species (any 3 colors)
plot(x = iris$Petal.Length, y = iris$Sepal.Length)

##  apply statements

# For the next few tasks you will use the list Cache500 
# (list of length 500, each element is a numeric vector of various lengths)
# LEAVE AS IS:
load("Cache500.rda")

# [3 pts]
# Create [first.cache], a vector where each entry is the _first_ element of the
# corresponding vector in the list Cache500

first.cache <- Cache500[1]


# [3 pts]
# Create [mean.cache], a vector of length 500 where each entry is the mean 
# of the corresponding element of the list Cache500

mean.cache <- mean(first.cache)


# [2 pts]
# Create [sd.cache], a vector of length 500 where each entry is the sd
# of the corresponding element of the list Cache500

sd.cache <- sd(first.cache)
  


# [4 pts]
# Create [mean.long.cache], a vector where 
# mean.long.cache[i] is:
# the mean of Cache500[[i]] IF it has 50 or more entries.
# NA IF Cache500[[i]] has less than 50 entries.

mean.long.cache <- <your code here>



