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
  result = tapply(y,x, function(y) sample(y,size = 10, replace = T))
  result = unlist(result)
  return(result)
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  nfit = length(fit)
  index = sample(1:nfit,size = nfit, replace = F)
  y = fit + err[index]
  return(y)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if(degree == 1){
    coeff = coef(lm(y ~ x))
  }
  if(degree == 2){
    coeff = coef(lm(y ~ x + I(x^2)))
  }
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

 
  ### Use fitModel to fit a model to this bootstrap Y
  if(is.null(fit)){
    new.y = genBootY(data[,1],data[,2])
    result = fitModel(data[,1],new.y,degree)
  }
  else{
    new.y = genBootR(fit[,1],fit[,2])
    result = fitModel(data[,1],new.y,degree)
  }
  return(result)
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  fit1 = fitModel(data[,1],data[,2]) # this gives me coefficient for linear model
  fit1y = data[,1] * fit1[2] + fit1[1] #fitted values for y
  fit.1 = data.frame(fit1y,fit1y - data[,2]) # error/ residuals
  
  fit2 = fitModel(data[,1],data[,2],degree = 2) #this gives me coefficients for the quad. model
  fit2y = (data[,1])^2 * fit2[3] + data[,1] * fit2[2] + fit2[1] #fitted values for y
  fit.2 = data.frame(fit2y,fit2y - data[,2]) #error/residuals

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  coeff = list()
  coeff[[1]] = t(replicate(B, oneBoot(data,degree = 1))) # linear w/o fit
  coeff[[2]] = t(replicate(B, oneBoot(data,fit.1,degree = 1))) # linear w/fit
  coeff[[3]] = t(replicate(B, oneBoot(data,degree = 2))) # quadratic w/o fit
  coeff[[4]] = t(replicate(B, oneBoot(data,fit.2,degree = 2))) # quadratic w/ fit
  
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  data = data.frame(x,y)
  coeff = as.data.frame(coeff)
  
  if(ncol(coeff) == 2){ #this plot for linear
    plot(y ~ x,xlab = "", ylab = "", ylim = c(1,40), pch = 20)
    mapply(abline,coeff[,1],coeff[,2],col =rgb(0,0,0.8,alpha = 0.05))
    curve(trueCoeff[1] + trueCoeff[2]*x + trueCoeff[3]*x^2, 1,6,xlab = "",
          ylab = "", ylim = c(1,40), col = "red",lwd = 5, add = T)
  }
  
  if(ncol(coeff) == 3){ #this plot for quadratic model
    plot(y ~ x, xlab = "", ylab = "", ylim = c(1,40), pch = 20)
    myfun = function(c1,c2,c3){
      curve(c1 + c2 *x + c3 *x^2,1,6,
            xlab = "", ylab = "", ylim = c(1,40),lwd = 0.5, add = T,
            ,col = rgb(0,0,0.8,alpha = 0.05))
    }
    
    mapply(myfun,coeff[,1],coeff[,2],coeff[,3])
    curve(trueCoeff[1] + trueCoeff[2]*x + trueCoeff[3]*x^2, 1,6,xlab = "",
          ylab = "", ylim = c(1,40), col = "red",lwd = 5, add = T)
  }
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
