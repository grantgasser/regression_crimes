### COMPLETE REGRESSION ON CRIMES ###

#read in data
crimes <- read.table('crimes.txt', header=T)

#swap x and y for visualization
dat <- data.frame(x=crimes[,2], y=crimes[,1])
dat

n <- nrow(dat)

attach(dat)

#get the fit for y (crimes) regressed on x
fit <- lm(y ~ x, data = dat)
summary(fit)

#plot data and regression line
par(mfrow=c(2,3))
plot(dat, col="blue")
abline(fit)


## Test regression model assumptions ##

#1: Test our assumption that the data is relatively linear
plot(fit, which=1)

#Diagnosis: looks pretty good, seems like linear fit is a good option here


#2: Test sigma^2. Is the variance of the errors constant?
#Can also see this in the residuals v. fitted plot
#plot(fit, which=1)

#Diagnosis: Though subjective, it looks like the variance of the errors is constant.
#For large and small values of y (lots of crime), there are not many data points so it is a bit
#difficult to tell. 

#Use bptest
library(lmtest)
bptest(fit) #H0: normality and independence of residuals

#pval = .9378, fail to reject, don't have enough evidence to say not variance is not constant

#3: Test independence of error terms
#Can also see with residuals v. fitted plot
#plot(fit, which=1)

#Diagnosis: There don't seem to be any trends up or down with errors, preserving our
#assumption of independence

#Use dwtest and bgtest
#before removing outliers
dwtest(fit) #H0: correlation is 0
bgtest(fit) #a bit more robust

#both tests are significant, suggesting there is correlation among the residuals
#and that they are not independant, contrary to my interpretation of the residuals plot

#4: Inspect outliers
#First plot reveals 27, 37 as outliers

#Look at 27
#MSE = 5,419,919
MSE <- mean(fit$residuals^2)
MSE
fit$residuals[27]^2 #46,284,412, much greater than MSE

#look at actual data
dat[27,]

#at x=78, regression line would predict (just by visual guess) about 7000-8000
#but y=14016 here

#can we throw this outlier away?
dat <- dat[-27,]

#Look at 36 (not 37 because we removed an outlier)
fit$residuals[37]^2 #27,860,561, much greater than MSE

#look at actual data
dat[36,]

#at x=77, model would predict a y near 7000-8000, but y=2105
#much less crime than expected

#can we throw this outlier away?
dat <- dat[-36,]

#fit and plot with outliers removed
fit <- lm(y~x, data=dat)
plot(fit, which=1)


#5: Are the error terms normally distributed?
library(car)
qqPlot(fit$residuals)

#Shapiro test for normality (H0: normally distributed)
shapiro.test(fit$residuals)

#even after removing 2 most severe outliers, still significant
#reject that the residuals are normally distributed


#CI and PI for the model
newdata <- data.frame(x=85)
predict(fit, newdata=newdata, interval="confidence")
predict(fit, newdata=newdata, interval="prediction")
