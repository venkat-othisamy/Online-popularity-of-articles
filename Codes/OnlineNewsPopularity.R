getwd()
setwd("E:/UMN BA/Intro to Statistics for Data Scientists/project")
library(readxl)

#Read the data
onlinepopularity<-read.csv('OnlineNewsPopularity.csv')

#Convert type of channel to a factor variable
onlinepopularity$channel<-factor(onlinepopularity$data_channel)


#Fit the multiple regression model(channel=2 as base category)
onlinepopularity<-within(onlinepopularity,channel<-relevel(channel,ref=3))
attach(onlinepopularity)

#simple linear model
linefit1a<-lm(shares~ channel+kw_avg_avg+kw_max_avg+self_reference_avg_sharess+ self_reference_min_shares+kw_min_avg+self_reference_avg_sharess+LDA_03+LDA_00+self_reference_min_shares+is_weekend+num_hrefs)
summary(linefit1a)

#check correlationship of predictors
cor(kw_avg_avg,kw_max_avg)
cor(self_reference_avg_sharess, self_reference_min_shares)

#Second model
linefit2a<-lm(shares~ channel+ num_keywords+kw_avg_avg+self_reference_avg_sharess+LDA_02+is_weekend+num_hrefs)
summary(linefit2a)

#Thrid model
linefit3a<-lm(shares~ channel+ num_keywords+kw_avg_avg+self_reference_avg_sharess+LDA_02+num_hrefs+ global_sentiment_polarity)
summary(linefit3a)

#Assumption Checking
linefit2a.stres <- rstandard(linefit2a)
plot(linefit2a$fitted.values, linefit2a.stres, pch = 16, main = "Standardized Residual Plot",xlab = "Fitted shares", ylab = "Standardized Residuals")
abline(0,0, lty=2, col="blue")

#histogram with normal curve
h <- hist(linefit2a.stres)
x <- linefit2a.stres
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")

# normal probability plot
qqnorm(linefit2a.stres,main = "Normal Probability Plot", xlab = "Normal Scores", ylab = "Standardized Residuals")
qqline(linefit2a.stres,col="red")



#Distribution of shares(y)
hist(shares)

#Use log(shares) as the response variable
#Change the scale of y
hist(log(shares))

#First model
linefit1<-lm(log(shares)~ channel+kw_avg_avg+kw_max_avg+self_reference_avg_sharess+ self_reference_min_shares+kw_min_avg+self_reference_avg_sharess+LDA_03+LDA_00+self_reference_min_shares+is_weekend+num_hrefs)
summary(linefit1)

#check correlationship of predictors
cor(kw_avg_avg,kw_max_avg)
cor(self_reference_avg_sharess, self_reference_min_shares)

#Second model
linefit2<-lm(log(shares)~ channel+ num_keywords+kw_avg_avg+self_reference_avg_sharess+LDA_02+LDA_00+is_weekend+num_hrefs)
summary(linefit2)

#Thrid model
linefit3<-lm(log(shares)~ channel+ num_keywords+kw_avg_avg+self_reference_avg_sharess+LDA_02+LDA_00+is_weekend+num_hrefs+ global_sentiment_polarity)
summary(linefit3)

#Assumption Checking
linefit2.stres <- rstandard(linefit2)
plot(linefit2$fitted.values, linefit2.stres, pch = 16, main = "Standardized Residual Plot",xlab = "Fitted shares", ylab = "Standardized Residuals")
abline(0,0, lty=2, col="blue")

#histogram with normal curve
h <- hist(linefit2.stres)
x <- linefit2.stres
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")

# normal probability plot
qqnorm(linefit2.stres,main = "Normal Probability Plot", xlab = "Normal Scores", ylab = "Standardized Residuals")
qqline(linefit2.stres,col="red")

#detach(onlinepopularity)

