# Lab 11: More about Plotting

# This lab aims at helping you with hw8 (plotting part) and hw9.

#### Plotting multiple lines
## Straight lines:
coeff <- data.frame(intercept = rnorm(1000, 5, 1),
                    slope = rnorm(1000, 2, 0.2))

# Make some phseudo-data
x <- 1:100
y <- 2*x+5+rnorm(100)

# Do the plot and add estimated lines:
plot(y~x, type="p")

?abline

mapply(abline, a=coeff[,1], b=coeff[,2], col=rgb(1,0.2,0.8,alpha=0.2))
# Use the 'rgb' color options, 'alpha' for transparency:
?rgb

## Quadratic lines:
coeff2 <- data.frame(intercept = rnorm(1000, 5, 1),
                     slope = rnorm(1000, 2, 0.2),
                     quadratic = rnorm(1000, 1, 0.1))

# Make some phseudo-data
x <- 1:100
y <- (x^2) + 2*x + 5 + rnorm(100)

# Do the plot and add estimated lines:
plot(y~x, type="p")

?curve

# Use a dummy function within mapply:
mapply(function(a,b,c){curve(a+b*x+c*(x^2),add=TRUE,col=rgb(1,0.2,0.8,alpha=0.2))}, 
       coeff2[,1],coeff2[,2],coeff2[,3])

# This is equivalent to:
drawCurve <- function(a,b,c){
  curve(a+b*x+c*(x^2),add=TRUE,col=rgb(1,0.2,0.8,alpha=0.2))
}

mapply(drawCurve, a=coeff2[,1], b=coeff2[,2], c=coeff2[,3])


#### Introduction to Bubble Chart and Motion Chart:

# https://developers.google.com/chart/interactive/docs/gallery/bubblechart
# https://developers.google.com/chart/interactive/docs/gallery/motionchart