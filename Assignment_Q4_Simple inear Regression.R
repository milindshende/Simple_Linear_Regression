###### Q4_Salary v/s Experience #########
library(readr)
sal_exp<-read.csv(file.choose())
View(sal_exp)

# Exploratory data analysis
summary(sal_exp)

#Scatter plot 
# plot(X,Y)
attach(sal_exp)
plot(YearsExperience,Salary)

str(sal_exp)
install.packages("lattice")
library(lattice)

#Correlation Coefficient (r)
cor(Salary,YearsExperience)  # cor (Y,X)

#Finding Missing Data
is.na(sal_exp)
sum(is.na(sal_exp))

# Simple Linear Regression model
reg_salexp <- lm(Salary ~ YearsExperience) # lm(Y ~ X)

summary(reg_salexp)

reg_salexp$residuals
reg_salexp$fitted.values
reg_salexp$coefficients
sum(reg_salexp$residuals)
mean(reg_salexp$residuals)

pred_salexp <- predict(reg_salexp)

sqrt(sum(reg_salexp$residuals^2)/nrow(sal_exp))  #RMSE

sqrt(mean(reg_salexp$residuals^2))


# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = sal_exp, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_exp, aes(x=YearsExperience, y=reg_salexp$fitted.values))

cor(reg_salexp$fitted.values,Salary)

#### Logrithamic Model#####

# x = log(yearsexp); y = salary

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)


reg_log_salexp <- lm(Salary ~ log(YearsExperience)) # lm(Y ~ X)

summary(reg_log_salexp)
predict(reg_log_salexp)

reg_log_salexp$residuals
reg_log_salexp$fitted.values
sqrt(sum(reg_log_salexp$residuals^2)/nrow(sal_exp))  #RMSE
sqrt(mean(reg_log_salexp$residuals^2))


confint(reg_log_salexp,level=0.95)
predict(reg_log_salexp,interval="confidence")

pred_log_salexp <- predict(reg_log_salexp)


# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = sal_exp, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_exp, aes(x=YearsExperience, y=reg_log_salexp$fitted.values))

cor(reg_log_salexp$fitted.values,Salary)

##### SQ model ####
# x = SQ(yearsExp); y = Salary

plot((YearsExperience*YearsExperience), Salary)
cor((YearsExperience*YearsExperience), Salary)

reg_sq_salexp <- lm(Salary ~ (YearsExperience*YearsExperience)) # lm(Y ~ X)

summary(reg_sq_salexp)
predict(reg_sq_salexp)

reg_sq_salexp$residuals
reg_sq_salexp$fitted.values
sqrt(sum(reg_sq_salexp$residuals^2)/nrow(sal_exp))  #RMSE
sqrt(mean(reg_sq_salexp$residuals^2))


confint(reg_sq_salexp,level=0.95)
predict(reg_sq_salexp,interval="confidence")

pred_sq_salexp <- predict(reg_sq_salexp)


# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = sal_exp, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_exp, aes(x=YearsExperience, y=reg_sq_salexp$fitted.values))

cor(reg_sq_salexp$fitted.values,Salary)

####### Exponential Model #####

# x = yearsexp and y = log(salary)

plot(YearsExperience, log(Salary))
cor(YearsExperience, log(Salary))

reg_exp_salexp <- lm(log(Salary) ~ YearsExperience) #lm(log(Y) ~ X)

summary(reg_exp_salexp)

reg_exp_salexp$residuals

sqrt(mean(reg_exp_salexp$residuals^2))

logsalexp <- predict(reg_exp_salexp)
salpred <- exp(logsalexp)
salpred

error = sal_exp$Salary - salpred
error

sqrt(sum(error^2)/nrow(sal_exp))  #RMSE

confint(reg_exp_salexp,level=0.95)
predict(reg_exp_salexp,interval="confidence") 

reg_exp_salexp$fitted.values

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = sal_exp, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_exp, aes(x=YearsExperience, y=salpred))

cor(salpred,Salary)


###### Polynomial model with 2 degree (quadratic model)#####

plot(YearsExperience, Salary)
plot(YearsExperience*YearsExperience,Salary)
cor(YearsExperience*YearsExperience,Salary)

plot(YearsExperience*YearsExperience,log(Salary))

cor(YearsExperience,log(Salary))

cor(YearsExperience*YearsExperience,log(Salary))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree_salexp <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience))

summary(reg2degree_salexp)

logpol_salexp <- predict(reg2degree_salexp)
expy_salexp <- exp(logpol_salexp)
err=Salary-expy_salexp
err

sqrt(sum(err^2)/nrow(sal_exp))  #RMSE

confint(reg2degree_salexp,level=0.95)
predict(reg2degree_salexp,interval="confidence")

reg2degree_salexp$fitted.values

# visualization
library(ggplot2)
ggplot(data = sal_exp, aes(x = YearsExperience + I(YearsExperience^2), y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_exp, aes(x=YearsExperience+I(YearsExperience^2), y=logpol_salexp))

cor(expy_salexp,Salary)
cor(logpol_salexp,Salary)
