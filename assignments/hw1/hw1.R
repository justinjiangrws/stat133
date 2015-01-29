##################################################################################
### Stat 133 Spring 2015
### Homework 1
### Due Sun Feb 1st by midnight
### but do try to complete it in section.

### NOTE: For this assignment you will be asked to create several variables in R.
### Please use THE EXACT VARIABLE NAMES given here.
### The variable names will be given inside double angle brackets (e.g. << x >>),
### this is solely so that you can quickly identify the variable name, it has NO
### meaning in R, and you should NOT include the brackets in your code.

### NOTE: Any line that starts with a # will not be executed by R.  You can add
### your own comments but make sure that anything that is part of you solution
### is not commented out.
##################################################################################
### Example (not part of assignment)

### You are asked to create the variable x, it could e.g. be
### any of the following (sans the # at the beginning):

# Create a variable:
# << x >> : a vector with the integers 1 through 100.

# x <- 1:100

### OR e.g. any of the following, 
# x = 1:100
# x <- seq(1, 100, by=1)
# x = seq(from=1, to=100, by=1)

### All of the four lines above are examples of valid solutions, you just have
### to provide one solution that works.  The only thing frowned upon would be
### to type out the answer, as in x <- c(1, 2, 3, 4, ... ,100)
##################################################################################
### Assignment starts
##################################################################################
### First part, variable assignment

# Create the variable
# << y >> : a vector of length 100 which has the even numbers from 2 to 200.
# For clarity put your code here, directly below the 


# Create the variable
# << z >> : a vector of length 20 with character entries, "hw1", "hw2", ..., "hw20"

# Create the variable
# << m >> : a vector of length 100 with entries from a standard normal distribution
set.seed(42)

# Create the variable
# << mean.m >> : a scalar, the mean of the numbers in << m >>

# Creat the variable
# << sd.m >> : a scalar, the standard deviation of the numbers in << m >>

# Create the variable
# << max.m >> : a scalar, the maximum of the numbers in << m >>


##################################################################################
### Second part, data frames

### NOTE: .rda and .RData are binary files used to save R variables for later use, 
### so you can save any variables in your R workspace with the save() function 
### and then later use load() to load them into R at a later date.

### Keep in mind that load() loads the file relative to the working directory
### so you need to go to 
###         Session > Set Working Directory > To Source File Location

# Load family.rda that is in your hw1 folder, the variable is called family:
load("family.rda")

### Check what data type the variable is with : class(family)
### now take a look at the data frame by typing family in the console.

# Create a new data frame 
# << family.men >> : a data frame that is a subset of family, with only the men


# Create a new data frame 
# << family.young >> : a data frame, subset of family, with only people *under* 40


# Create a new data frame 
# << family.30y68i >> : a data frame, subset of family, with only people *over* 30, *shorter* than 68 in


# Formula for BMI : BMI = (weight in lbs) / (height in in)^2 * 703
# Note: the dataframe has weight in lbs and height in in as required.
# Create a new variable 
# << bmi >> : a vector with the BMI of each family member 


# Create a new data frame
# << family2 >> : family with an added column of BMI, with column name bmi



##################################################################################
