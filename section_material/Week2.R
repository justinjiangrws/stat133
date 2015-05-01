##########Subsetting a dataframe###########
mtcars
colnames(mtcars)

##Subset by columns
#Suppose we want the first column:
mtcars[,1]
mtcars$mpg
#Suppose we want every column except for the first one:
mtcars[,-1]

##Subset by rows
#Suppose we want the first ten rows:
mtcars[1:10, ]
#Suppose we want the rows where hp is bigger than 100:
mtcars[mtcars$hp > 100, ]
#Suppose we want the rows where gear equals 4:
mtcars[mtcars$gear == 4, ] #Note the "=="

################Scatterplot & Histogram###################
#Suppose we want to plot mpg against hp:
plot(x = mtcars$hp, y = mtcars$mpg)
#To add titles:
plot(x = mtcars$hp, y = mtcars$mpg, xlab = "Horsepower", ylab = "MPG",
     main = "MPG v.s. Horsepower")

#Suppose we want to check the distribution of mpg:
hist(mtcars$mpg)