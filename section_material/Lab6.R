# Lab 6: Browser(), Image() and Vectorized Calculations

# 6.1 Using browser() to debug a function
# Putting browser() inside of a function allows you to interrupt the execution
# and check the variables in the environment at the point where browser() was inserted.
# Use 'c' to exit browser mode.

# Example 6.1:
foo <- function(x, y){
  z <- x*y
  browser()
  return(z*sqrt(z))
}

foo(3, 4)

# 6.2 Using image() to visualize a matrix
# image() allows you to visualize a matrix via a color grid (like a heat map),
# with the matrix values corresponding to pixel intensity on image.

# Example 6.2: to have a sense of what image does:
# In this project you are using 0 for no cars, 1 for red cars and 2 for blue cars.
mat <- matrix(c(2,0,0,1), 2, 2)
mat
image(mat)

# x axis corresponds to row number, y axis corresponds to column number, with column 1 at bottom.
# A 90-degree counter-clockwise rotation of your original matrix.
# So this visualization here corresponds to:
#  0   1
#  2   0
text(x=c(0,0,1,1), y=c(0,1,0,1), labels=c(2,0,0,1), cex=3)
# Remember dark red (instead of lighter colors) represents vacancy/less crowded,
# if you use the default color options of image().

# Example 6.3: image() is a straightforward way to visualize matrices, especially for big ones:
big.mat <- matrix(rnorm(100), 10, 10)
big.mat
image(big.mat)

bigger.mat <- matrix(rnorm(10000), 100, 100)
View(bigger.mat)
image(bigger.mat)

# 6.3 Vectorized Calculations in R
# Use vectorized calculations to avoid loops and make things faster.

# Example 6.4
a <- c(1,2,3,4,5)
a^2
#What will happen?

# Example 6.5: Vectorized v.s. For Loop
x <- c(1,2,3)
y <- c(4,5,6)

# Suppose we want to have z[i] = x[i] + y[i]
# In other programming languages, this is done using a for loop:
z <- rep(0,3)
for (i in 1:3){z[i] <- x[i] + y[i]}
z

# But in R this is simply done by x+y:
z <- x + y
z

# Example 6.6: Dot Product
# Write a function that returns the dot product of two vectors.
# Remember for dot product AB = A1B1 + A2B2 + A3B3 + ......

dotProd <- function(x,y){ #Using loop
  if (length(x) != length(y)){stop("x and y must have same length")}
  z <- rep(0, length(x))
  for (i in 1:length(x)){
    z[i] <- x[i]*y[i]
  }
  return(sum(z))
}

dotProd_vec <- function(x,y){ #Using vectorized calculation
  if (length(x) != length(y)){stop("x and y must have same length")}
  return(sum(x*y))
}

#Some test
a <- rnorm(100)
b <- runif(100)

dotProd(a,b)
dotProd_vec(a,b)
#Same result. 