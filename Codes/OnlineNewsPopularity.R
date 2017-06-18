getwd()
setwd("E:/UMN BA/Intro to Statistics for Data Scientists/project")
library(readxl)

#Read the data
onlinepopularity<-read.csv('OnlineNewsPopularity.csv')

#Convert type of channel to a factor variable
onlinepopularity$channel<-factor(onlinepopularity$data_channel)


#Fit the multiple regression model(channel=6 as base category)
onlinepopularity<-within(onlinepopularity,channel<-relevel(channel,ref=3))
attach(onlinepopularity)

#Distribution of shares(y)
hist(shares,main= 'Number of Shares', xlab="Shares",ylab='Frequency')

#Use log(shares) as the response variable
#Change the scale of y
hist(log(shares),main= 'Number of Shares', xlab="Shares",ylab='Frequency')

#First model
linefit1<-lm(log(shares)~ channel+ num_keywords+kw_avg_avg+self_reference_avg_sharess+LDA_02+LDA_00+is_weekend+num_hrefs)
summary(linefit1)

#Second model
linefit2<-lm(log(shares)~ channel+ num_keywords+kw_avg_avg+self_reference_avg_sharess+is_weekend+num_hrefs)
summary(linefit2)

#Assumption Checking
linefit2.stres <- rstandard(linefit2)
plot(linefit2$fitted.values, linefit2.stres, pch = 16, main = "Standardized Residual Plot",xlab = "Fitted shares", ylab = "Standardized Residuals")
abline(0,0, lty=2, col="blue")

#histogram with normal curve
h <- hist(linefit2.stres,main = "Histogram of Model Residuals",xlab='Model Residuals')
x <- linefit2.stres
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")

# normal probability plot
qqnorm(linefit2.stres,main = "Normal Probability Plot", xlab = "Normal Scores", ylab = "Standardized Residuals")
qqline(linefit2.stres,col="red")

detach(onlinepopularity)
