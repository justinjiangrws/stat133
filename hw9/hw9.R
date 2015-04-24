# Homework 9
# Stat 133, Lec 2, Spring 2015
# Due : May 1st, Friday

# In this homework you will make a "bubble chart".  They were 
# used in excellent TED talks by Hans Rosling, which are worth a look:
# https://www.ted.com/speakers/hans_rosling

# Rosling and collegues founded gapminder and later Google incorporated
# their "bubble graphs" in their tools:
# http://www.gapminder.org/tag/google/
# https://developers.google.com/chart/interactive/docs/gallery/bubblechart

# In this homework we are going to use the tools in the R package 'googleVis'
# to make bubble charts and motion charts.
# You need to first install the package:

# install.packages('googleVis')  
# NOTE: you should execute the line above once, then keep it commented out.

# Now open the library:
library('googleVis')

##### Bubble chart:
# For examples of bubble chart, see: 
# https://developers.google.com/chart/interactive/docs/gallery/bubblechart
# To do this in R, use the 'gvisBubbleChart' function. 
# Make sure you read and understand its arguments:

?gvisBubbleChart

# For this bubble chart we will use the built-in dataset 'mtcars':
head(mtcars)
# Introduce a new column to the dataset, 
# The column should be called "model" and it should have the names of car models 
# (i.e. the current row names).

# < your code here>

# Now make a bubble chart using the following instructions:
# Use 'model', i.e. car model names as labels of bubbles;
# Use 'disp' as x axis and 'mpg' as y axis;
# use 'hp' to represent size of bubbles;
# Use levels of 'gear' to represent color of bubbles;
# Finally use the 'options' argument to add axis labels and main title.

# optionlist <- < your code here>
# bub <- gvisBubbleChart( < your code here >)

# Now plot your bubble chart output, 'bub', 
# the chart will show up in a new tab in your web browser.

# < your code here>

##### Motion Chart
# For examples of motion chart, see: 
# https://developers.google.com/chart/interactive/docs/gallery/motionchart
# To do this in R, use the 'gvisMotionChart' function. 
# Make sure you read and understand its arguments:

?gvisMotionChart

# We will be using the 'WorldBank' dataset containing information of countries,
# such as fertility rate, life expectancy, population, GDP per capita, etc.
load("WorldBank.RData")

# First subset the data frame and create a new data frame called <WorldDat>,
# containing only the following columns from WordBank:
# country, year, fertility rate, life expectancy, population and region.

# WorldDat <- < your code here>

# As you can see, there are missing values in this data frame.
# Get rid of all rows with one or more NAs.

# < your code here >

# Now make the motion chart using <WorldDat>:
# (at this point is should have 6 columns and should be free of missing values)
# Plot life expectancy against fertility rate for each country, 
# with 'year' as the time dimension, 'region' as the color vector, 'population' as the size vector.

# Notice that you can change theses vectors on the generated motion chart, 
# for now just use the above instructions as default.

# Motion <- gvisMotionChart( < your code here > )

# Plot your motion chart. It should appear in your web browser. Play around with it!
plot(Motion)
