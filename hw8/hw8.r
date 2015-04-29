xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  xval <- unique(x)
  yval <- c()
  for (i in 1:length(xval)){
    amount <- length(grep(xval[i],x))
    new_y <- c()
    for (j in 1:length(xval)){
      new_y <- c(new_y,yval[grep(xval[i],x)])
      value[grep(xval[i],x)] <- sample(new_y, size = amount, replace = TRUE)
    }
  }
  return (yval)
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  error <- sample(err, length = length(fit), replace = TRUE)
  y <- fit + error
  return (y)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree == 1){
    coeff <- lm(y~x)
  }
  if (degree > 1){
    coeff <- lm(y~x + I(x^2)) 
  }
  coeffasvec <- as.vector(coeff$coefficients)
  return(coeffasvec)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

 
  ### Use fitModel to fit a model to this bootstrap Y 
  if (fit == NULL){
    y <- genBootY(data[,1],data[,2])
    myModel <- fitModel(data[,1],y,degree = degree)
  }
  else{
    y <- genBootR(fit[,1],fit[,2])
    myModel <- fitModel(data[,1],y,degree = degree)
  }
  return(myModel)
}


repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  
  x <- data$x
  y <- data$y

  line <- lm(y~x)$fitted.values
  quadratic <- lm(y ~ x + I(x^2))$fitted.values
  
  residuals.line <- lm(y~x)$residuals
  residuals.quadratic <- lm(y ~ x + I(x^2))$residuals
  
  fit.line <- data.frame(line,residuals.line)
  fit.quadratic <- data.frame(quadratic,residuals.quadratic)
  
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  r1 <- replicate(n = B,oneBoot(data,fit = NULL,degree = 1))
  r2 <- replicate(n = B,oneBoot(data,fit = NULL,degree = 2))
  r3 <- replicate(n = B,oneBoot(data,fit = fit.line,degree = 1))
  r4 <- replicate(n = B,oneBoot(data,fit = fit.quadratic,degree = 2))
  
  myValue <- list(r1,r2,r3,r4)
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  
  return(myValue)
} 

## recieved some help from Julia Gomes here

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data

  plot(y~x)
  
  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  curve(trueCoeff[1]+trueCoeff[2]*x + trueCoeff[3]*(x^2),col="blue",add=TRUE,lwd=3)
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  
  
}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
