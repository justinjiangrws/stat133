#####Use par() to set default options####
?par
par() #To check current settings

##Plot two graphs side-by-side:
par(mfrow = c(1,2))
plot(mpg~hp, data=mtcars)
plot(mpg~wt, data=mtcars)

##Plot two graphs up and down:
par(mfrow = c(2,1))
par(mar = c(3,2,1.5,1.5)) #Set the margins
plot(mpg~hp, data=mtcars)
plot(mpg~wt, data=mtcars)

##Don't forget to set back:
par(mfrow = c(1,1), mar = c(5,4,4,2)+0.1)
plot(mpg~hp, data=mtcars)

######Add lines to a graph######
plot(mpg~hp, data=mtcars)
##Suppose we want a vertical line representing hp=200:
?lines
?abline

n = nrow(mtcars)
lines(x = rep(200, n), y = mtcars$mpg, col="red")
#Or equivalently:
abline(v = 200, col="red") #Check what "h" means here

########Add text to a graph######
?text
plot(mpg~hp, data=mtcars)
text(x=250, y=25, labels="test")
##Use arguments to adjust color and size:
plot(mpg~hp, data=mtcars)
text(x=250, y=25, labels="test", cex=5, col="blue")
