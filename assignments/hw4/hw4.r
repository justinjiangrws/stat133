# HW 4
# Writing functions
# Due Thursday February 26th by midnight 
# This .r file should contain your code

#### Function #1
# Implement the function "listLengths". 

# Input variable:
# <data.list>: a list whose elements are vectors of varying length

# Output variable:
# <element.lengths>: a numeric vector whose entries are the lengths of each
#   element of <data.list>

listLengths <- function(data.list) {

    # your code here

}

#### Function 2
#### Implement the function "powers"

# Input variable :
# <x> : a numeric vector of length n
# <k> : an integer

# Output variable
# <x.powers> : A matrix of size [n x k] where the first column is x, the second column x^2, the third column x^4, etc.
#              the column names should be : "x", "x^2", "x^3" etc.

powers <- function(x, k){

}

 
#### Function #3
#### Implement the function "recipeConversion"

# Input variable:
# <recipe> : A data frame with three columns named "amount", "unit" and "ingredient"

# Output variable:
# <recipe.metric> : A data frame with three columns where cups have been converted to ml and ounces to grams.
#                   the number in "amount" should be updated, and the entry in "unit" changed
#                   both ml and gr should be rounded to the nearest multiple of 5,
#                   e.g. a row that read : [2 cups flour] should now be [475 ml flour]
#                   Note, if the "unit" is neither "cup"/"cups" nor "oz" the row should not be changed

# The conversion constants are: 
# 1 cup = 236.6 ml and 1 oz = 28.3 gr
# Please use these exact numbers, do not add decimal places.

# "unit" can take any of a number of values but you need to find the rows where
# "unit" is : "cup", "cups" or "oz"

# Note: to find a match in "unit" you have a few different options, you can go row by row
# and check if the unit is equal to cup/cups/oz using the "==" operator, you can use the
# match() or %in% operators or finally you can look at the function grep(). 

# If the column names of the input data frame are not "amount", "unit" and "ingredient" the
# function should stop and print out an error message

# Put your code here
recipeConversion <- function(recipe){

}


#### Function #4a
# Implement the function "bootstrapVarEst"

# Input variable:
# <x> : data vector
# <B> : the number of boostrap iterations

# Output variable:
# <boot.sigma2.est> : Bootstrap estimate for the variance of the sample mean (see lecture notes)

# The bootstrap is a resampling method used here to estimate the variance of the sample mean.
# Given a sample, X_1, X_2, ..., X_n of observations we want to estimate the mean (expected value)
# of the distrubution of the random variables X, and we want an estimate of the variance of that 
# sample mean (note, not the variance of the X_i but the variance of the sample mean).
# We do this by:
# -- Setting B, the number of bootstrap iterations (typically 1000, 5000 or 10000)
# -- For each i in 1:B we draw a new sample (called bootstrap sample) of size n from the original sample
#    i.e. we draw n values at random *with replacement* from the observations X_1, ..., X_n. (use sample())
# -- Calculate, and store, the mean of this bootstrap sample, call that mu_i (i in 1:B)
# -- The bootstrap variance is the sample variance of mu_1, mu_2, ..., mu_B

bootstrapVarEst <- function(x, B){

}

#### Function #4b
#### Implement the function "jackknifeVarEst"

# Input variable:
# <x> : data vector

# Output variable:
# <jack.sigma2.est> : Jackknife estimate for the variance of the sample mean (see lecture notes)

# The jackknife is a resampling method used here to estimate the variance of the sample mean.
# Given a sample, X_1, X_2, ..., X_n of observations we want to estimate the mean (expected value)
# of the distrubution of the random variables X, and we want an estimate of the variance of that 
# sample mean (note, not the variance of the X_i but the variance of the sample mean).
# We do this by:
# -- For each i in 1:n we omit the i-th observation (i.e. take the sample X_1, ..., X_[i-1], X_[i+1], ..., X_n)
#     for this reduced sample calculate the sample mean (get mu_1, mu_2, ..., mu_n)
# -- The jackknife variance is the sample variance of mu_1, mu_2, ..., mu_n

jackknifeVarEst <- fuction(x){

}

#### Function #4c
#### Implement the function "samplingVarEst"

# Input variables:
# <x> : data vector
# <type> : string that takes the values "bootstrap" or "jackknife", the default should be bootstrap.

# Output variable:
# <sampling.sigma.est> : The bootstrap estimate if type="bootstrap" and the jackknife estimate if type="jackknife"

# Note: this function calls the previous two functions.

samplingVarEst <- function(  ){

}


