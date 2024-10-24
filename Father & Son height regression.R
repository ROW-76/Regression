#Loading the data from the library 'Using R' and specifying the data set.
library(UsingR);
data(father.son);

#Plotting the values of the given data set in the scatter plot.
plot(father.son$fheight, father.son$sheight,
xlab="Father's height (in)",
ylab="Son's height (in)",
pch=1);

#Adding the Regression line on the scatter plot
sons_h <- father.son$sheight
fathers_h <- father.son$fheight
regression_line <- lm(sons_h ~ fathers_h, data = father.son)
abline(regression_line, col='red')

#Adding the SD line
slope_SF <- sd(sons_h)/ sd(fathers_h)
mean_S <- mean(sons_h)
mean_F <- mean(fathers_h)
#using the straight line equation y-y1 = m(x-x1)
x <- 0 #x by default will be zero if interception in concerned
intercept <- slope_SF * (x - mean_F )+ mean_S
abline(a= intercept, b = slope_SF, col='blue', lty=4, lwd=3)

#Marking the center point of regression
points(mean(fathers_h), mean(sons_h) ,
col='yellow',
pch= 20)

#Extending the X & Y axis line through the center of regression
abline(v=mean(fathers_h), col="green")
abline(h=mean(sons_h), col="green")

#Out put of Linear regression.
summary(regression_line)