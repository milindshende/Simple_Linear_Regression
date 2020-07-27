###### Q1_Calory v/s weight #########
library(readr)
cal_wt<-read.csv(file.choose())
View(cal_wt)

# Exploratory data analysis
summary(cal_wt)

#Scatter plot 
# plot(X,Y)
plot(cal_wt$Calories.Consumed,cal_wt$Weight.gained..grams.)

attach(cal_wt)
str(cal_wt)
install.packages("lattice")
library(lattice)

#Correlation Coefficient (r)
cor(Weight.gained..grams.,Calories.Consumed)      # cor(Y,X)

#Finding Missing Data
is.na(cal_wt)
sum(is.na(cal_wt))
sum(is.na(cal_wt$Calories.Consumed))
sum(is.na(cal_wt$Weight.gained..grams.))

# Simple Linear Regression model
reg_calwt <- lm(Weight.gained..grams. ~ Calories.Consumed) # lm(Y ~ X)

summary(reg_calwt)

reg_calwt$residuals
reg_calwt$fitted.values
reg_calwt$coefficients
sum(reg_calwt$residuals)
mean(reg_calwt$residuals)

pred_calwt <- predict(reg_calwt)


sqrt(sum(reg_calwt$residuals^2)/nrow(cal_wt))  #RMSE

sqrt(mean(reg_calwt$residuals^2))


# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = cal_wt, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal_wt, aes(x=Calories.Consumed, y=reg_calwt$fitted.values))

cor(reg_calwt$fitted.values,Weight.gained..grams.)
cor(Weight.gained..grams.,Weight.gained..grams.)

#### Logrithamic Model#####

# x = log(Cal); y = Weight

plot(log(Calories.Consumed), Weight.gained..grams.)
cor(log(Calories.Consumed), Weight.gained..grams.)
cor(Weight.gained..grams.,log(Calories.Consumed))

reg_log_calwt <- lm(Weight.gained..grams. ~ log(Calories.Consumed)) # lm(Y ~ X)

summary(reg_log_calwt)
predict(reg_log_calwt)

reg_log_calwt$residuals
reg_log_calwt$fitted.values
sqrt(sum(reg_log_calwt$residuals^2)/nrow(cal_wt))  #RMSE
sqrt(mean(reg_log_calwt$residuals^2))


confint(reg_log_calwt,level=0.95)
predict(reg_log_calwt,interval="confidence")




pred_log_calwt <- predict(reg_log_calwt)


# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = cal_wt, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal_wt, aes(x=Calories.Consumed, y=reg_log_calwt$fitted.values))

cor(reg_log_calwt$fitted.values,Weight.gained..grams.)

##### SQRT model ####
# x = SQRT(Cal); y = Weight
?sqrt

plot(sqrt(Calories.Consumed), Weight.gained..grams.)
cor(sqrt(Calories.Consumed), Weight.gained..grams.)
cor(Weight.gained..grams.,sqrt(Calories.Consumed))

reg_sqrt_calwt <- lm(Weight.gained..grams. ~ sqrt(Calories.Consumed)) # lm(Y ~ X)

summary(reg_sqrt_calwt)
predict(reg_sqrt_calwt)

reg_sqrt_calwt$residuals
reg_sqrt_calwt$fitted.values
sqrt(sum(reg_sqrt_calwt$residuals^2)/nrow(cal_wt))  #RMSE
sqrt(mean(reg_sqrt_calwt$residuals^2))


confint(reg_sqrt_calwt,level=0.95)
predict(reg_sqrt_calwt,interval="confidence")

pred_sqrt_calwt <- predict(reg_sqrt_calwt)


# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = cal_wt, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal_wt, aes(x=Calories.Consumed, y=reg_sqrt_calwt$fitted.values))

cor(reg_sqrt_calwt$fitted.values,Weight.gained..grams.)

####### Exponential Model #####

# x = Cal and y = log(Weight)

plot(Calories.Consumed, log(Weight.gained..grams.))

cor(Calories.Consumed, log(Weight.gained..grams.))

reg_exp_calwt <- lm(log(Weight.gained..grams.) ~ Calories.Consumed) #lm(log(Y) ~ X)

summary(reg_exp_calwt)

reg_exp_calwt$residuals

sqrt(mean(reg_exp_calwt$residuals^2))

logcalwt <- predict(reg_exp_calwt)
wtgainedgrams <- exp(logcalwt)

error = cal_wt$Weight.gained..grams. - wtgainedgrams
error

sqrt(sum(error^2)/nrow(cal_wt))  #RMSE

confint(reg_exp_calwt,level=0.95)
predict(reg_exp_calwt,interval="confidence") 

reg_exp_calwt$fitted.values

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = cal_wt, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal_wt, aes(x=Calories.Consumed, y=reg_exp_calwt$fitted.values))

cor(reg_exp_calwt$fitted.values,Weight.gained..grams.)


###### Polynomial model with 2 degree (quadratic model)#####

plot(Calories.Consumed, Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)

cor(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)

plot(Calories.Consumed*Calories.Consumed,log(Weight.gained..grams.))

cor(Calories.Consumed,log(Weight.gained..grams.))

cor(Calories.Consumed*Calories.Consumed,log(Weight.gained..grams.))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree_calwt <- lm(log(Weight.gained..grams.) ~ Calories.Consumed + I(Calories.Consumed*Calories.Consumed))

summary(reg2degree_calwt)

logpol_calwt <- predict(reg2degree_calwt)
expy_calwt <- exp(logpol_calwt)

err = cal_wt$Weight.gained..grams. - expy_calwt

sqrt(sum(err^2)/nrow(cal_wt))  #RMSE

confint(reg2degree_calwt,level=0.95)
predict(reg2degree_calwt,interval="confidence")

reg2degree_calwt$fitted.values

# visualization
library(ggplot2)
ggplot(data = cal_wt, aes(x = Calories.Consumed + I(Calories.Consumed^2), y = log(Weight.gained..grams.))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal_wt, aes(x=Calories.Consumed+I(Calories.Consumed^2), y=logpol_calwt))

cor(reg2degree_calwt$fitted.values,Weight.gained..grams.)
