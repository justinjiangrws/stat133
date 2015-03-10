# Please load in the dataset family-quiz.rda. It will be
# required to perform the following tasks. The dataset includes data for
# a family.


# calculate the mean and median age in the family. Store these as the
# variables <mean.age> and <med.age> respectively.

# mean.age <- your code here
# med.age <- your code here


# For each person in the dataset, calculate the squared difference between its
# height and the mean height of the family. Store this as the variable
# <height.diffs>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset

# height.diffs <- your code here



# Please create the following two data frames and store them with the indicated names:
# 1) people whose age is strictly greater than <mean.age>:  <ppl.old>
# 2) people whose age is less than or equal to <mean.age>: <ppl.young>

# ppl.old <- your code here
# ppl.young <- your code here

# For each of your subsets, create a vector giving the weight of each person. Name
# these variables <ppl.old.weight> and <ppl.young.weight>.

# ppl.old.weight <- your code here
# ppl.young.weight <- your code here


# Please implement the function bmiByheight. Your function should take the
# following arguments:
#
# <height.range>: a numeric vector of length 2 whose first and second elements
#   give the minimum height and maximum height to consider
# <height>: a numeric vector giving the height of each person
# <bmi>: a numeric vector giving the bmi of each person associated with <height>
#   (this should be the same length as <height>)
#
# Your function should return the average of <bmi> for all observations with
# <height> in the range (inclusive) specified by <height.range>

bmiByheight <- function(height.range, height, bmi) {
  
  # your code here
  
}


# Please create a plot of bmi (y-axis) against age (x-axis). Your plot
# should include the following features:
# 1) a title "BMI vs Age"
# 2) axis labels: "BMI" and "Age (Year)"
# 3) a blue line with intercept= 22.89050 and slope= 0.03501

