###### Q3_Salary Hike v/s Churn out rate #########
library(readr)
sal_ch<-read.csv(file.choose())
View(sal_ch)

# Exploratory data analysis
summary(sal_ch)

#Scatter plot 
# plot(X,Y)
attach(sal_ch)
plot(Salary_hike,Churn_out_rate)

str(sal_ch)
install.packages("lattice")
library(lattice)

#Correlation Coefficient (r)
cor(Churn_out_rate,Salary_hike)  # cor (Y,X)

#Finding Missing Data
is.na(sal_ch)
sum(is.na(sal_ch))

# Simple Linear Regression model
reg_salch <- lm(Churn_out_rate ~ Salary_hike) # lm(Y ~ X)

summary(reg_salch)

reg_salch$residuals
reg_salch$fitted.values
reg_salch$coefficients
sum(reg_salch$residuals)
mean(reg_stdt$residuals)

pred_salch <- predict(reg_salch)

sqrt(sum(reg_salch$residuals^2)/nrow(sal_ch))  #RMSE

sqrt(mean(reg_salch$residuals^2))


# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = sal_ch, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_ch, aes(x=Salary_hike, y=reg_salch$fitted.values))

cor(reg_salch$fitted.values,Churn_out_rate)

#### Logrithamic Model#####

# x = log(sal); y = churnout

plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)


reg_log_salch <- lm(Churn_out_rate ~ log(Salary_hike)) # lm(Y ~ X)

summary(reg_log_salch)
predict(reg_log_salch)

reg_log_salch$residuals
reg_log_salch$fitted.values
sqrt(sum(reg_log_salch$residuals^2)/nrow(sal_ch))  #RMSE
sqrt(mean(reg_log_salch$residuals^2))


confint(reg_log_salch,level=0.95)
predict(reg_log_salch,interval="confidence")

pred_log_salch <- predict(reg_log_salch)


# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = sal_ch, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_ch, aes(x=Salary_hike, y=reg_log_salch$fitted.values))

cor(reg_log_salch$fitted.values,Churn_out_rate)

##### SQRT model ####
# x = SQRT(sal); y = churnrate
?sqrt

plot(sqrt(Salary_hike), Churn_out_rate)
cor(sqrt(Salary_hike), Churn_out_rate)

reg_sqrt_salch <- lm(Churn_out_rate ~ sqrt(Salary_hike)) # lm(Y ~ X)

summary(reg_sqrt_salch)
predict(reg_sqrt_salch)

reg_sqrt_salch$residuals
reg_sqrt_salch$fitted.values
sqrt(sum(reg_sqrt_salch$residuals^2)/nrow(sal_ch))  #RMSE
sqrt(mean(reg_sqrt_salch$residuals^2))


confint(reg_sqrt_salch,level=0.95)
predict(reg_sqrt_salch,interval="confidence")

pred_sqrt_salch <- predict(reg_sqrt_salch)


# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = sal_ch, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_ch, aes(x=Salary_hike, y=reg_sqrt_salch$fitted.values))

cor(reg_sqrt_salch$fitted.values,Churn_out_rate)

####### Exponential Model #####

# x = sal and y = log(churn)

plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))

reg_exp_salch <- lm(log(Churn_out_rate) ~ Salary_hike) #lm(log(Y) ~ X)

summary(reg_exp_salch)

reg_exp_salch$residuals

sqrt(mean(reg_exp_salch$residuals^2))

logsalch <- predict(reg_exp_salch)
churn <- exp(logsalch)

error = sal_ch$Churn_out_rate - churn
error

sqrt(sum(error^2)/nrow(sal_ch))  #RMSE

confint(reg_exp_salch,level=0.95)
predict(reg_exp_salch,interval="confidence") 

reg_exp_salch$fitted.values

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = sal_ch, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_ch, aes(x=Salary_hike, y=churn))

cor(churn,Churn_out_rate)


###### Polynomial model with 2 degree (quadratic model)#####

plot(Salary_hike, Churn_out_rate)
plot(Salary_hike*Salary_hike, Churn_out_rate)
cor(Salary_hike*Salary_hike, Churn_out_rate)

plot(Salary_hike*Salary_hike, log(Churn_out_rate))
cor(Salary_hike,log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree_salch <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))

summary(reg2degree_salch)

logpol_salch <- predict(reg2degree_salch)
expy_salch <- exp(logpol_salch)
err= Churn_out_rate-expy_salch
err

sqrt(sum(err^2)/nrow(sal_ch))  #RMSE

confint(reg2degree_salch,level=0.95)
predict(reg2degree_salch,interval="confidence")

reg2degree_salch$fitted.values

# visualization
library(ggplot2)
ggplot(data = sal_ch, aes(x = Salary_hike + I(Salary_hike^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_ch, aes(x=Salary_hike+I(Salary_hike^2), y=logpol_salch))

cor(expy_salch,Churn_out_rate)
cor(logpol_salch,Churn_out_rate)
