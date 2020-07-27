###### Q2_Delivery time v/s Sorting time #########
library(readr)
st_dt<-read.csv(file.choose())
View(st_dt)

# Exploratory data analysis
summary(st_dt)

#Scatter plot 
# plot(X,Y)
plot(st_dt$Sorting.Time,st_dt$Delivery.Time)

attach(st_dt)
str(st_dt)
install.packages("lattice")
library(lattice)

#Correlation Coefficient (r)
cor(Delivery.Time,Sorting.Time)      # cor(Y,X)

#Finding Missing Data
is.na(st_dt)
sum(is.na(st_dt))

# Simple Linear Regression model
reg_stdt <- lm(Delivery.Time ~ Sorting.Time) # lm(Y ~ X)

summary(reg_stdt)

reg_stdt$residuals
reg_stdt$fitted.values
reg_stdt$coefficients
sum(reg_stdt$residuals)
mean(reg_stdt$residuals)

pred_stdt <- predict(reg_stdt)


sqrt(sum(reg_stdt$residuals^2)/nrow(st_dt))  #RMSE

sqrt(mean(reg_stdt$residuals^2))


# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = st_dt, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = st_dt, aes(x=Sorting.Time, y=reg_stdt$fitted.values))

cor(reg_stdt$fitted.values,Delivery.Time)

#### Logrithamic Model#####

# x = log(st); y = dt

plot(log(Sorting.Time), Delivery.Time)
cor(log(Sorting.Time), Delivery.Time)

reg_log_stdt <- lm(Delivery.Time ~ log(Sorting.Time)) # lm(Y ~ X)

summary(reg_log_stdt)
predict(reg_log_stdt)

reg_log_stdt$residuals
reg_log_stdt$fitted.values
sqrt(sum(reg_log_stdt$residuals^2)/nrow(st_dt))  #RMSE
sqrt(mean(reg_log_stdt$residuals^2))


confint(reg_log_stdt,level=0.95)
predict(reg_log_stdt,interval="confidence")

pred_log_stdt <- predict(reg_log_stdt)


# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = st_dt, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = st_dt, aes(x=Sorting.Time, y=reg_log_stdt$fitted.values))

cor(reg_log_stdt$fitted.values,Delivery.Time)

##### SQRT model ####
# x = SQRT(st); y = dt
?sqrt

plot(sqrt(Sorting.Time), Delivery.Time)
cor(sqrt(Sorting.Time), Delivery.Time)

reg_sqrt_stdt <- lm(Delivery.Time ~ sqrt(Sorting.Time)) # lm(Y ~ X)

summary(reg_sqrt_stdt)
predict(reg_sqrt_stdt)

reg_sqrt_stdt$residuals
reg_sqrt_stdt$fitted.values
sqrt(sum(reg_sqrt_stdt$residuals^2)/nrow(st_dt))  #RMSE
sqrt(mean(reg_sqrt_stdt$residuals^2))


confint(reg_sqrt_stdt,level=0.95)
predict(reg_sqrt_stdt,interval="confidence")

pred_sqrt_stdt <- predict(reg_sqrt_stdt)


# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = st_dt, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = st_dt, aes(x=Sorting.Time, y=reg_sqrt_stdt$fitted.values))

cor(reg_sqrt_stdt$fitted.values,Delivery.Time)

####### Exponential Model #####

# x = st and y = log(dt)

plot(Sorting.Time, log(Delivery.Time))
cor(Sorting.Time, log(Delivery.Time))

reg_exp_stdt <- lm(log(Delivery.Time) ~ Sorting.Time) #lm(log(Y) ~ X)

summary(reg_exp_stdt)

reg_exp_stdt$residuals

sqrt(mean(reg_exp_stdt$residuals^2))

logstdt <- predict(reg_exp_stdt)
dt <- exp(logstdt)

error = st_dt$Delivery.Time - dt
error

sqrt(sum(error^2)/nrow(st_dt))  #RMSE

confint(reg_exp_stdt,level=0.95)
predict(reg_exp_stdt,interval="confidence") 

reg_exp_stdt$fitted.values

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = st_dt, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = st_dt, aes(x=Sorting.Time, y=reg_exp_stdt$fitted.values))

cor(reg_exp_stdt$fitted.values,Delivery.Time)


###### Polynomial model with 2 degree (quadratic model)#####

plot(Sorting.Time, Delivery.Time)
plot(Sorting.Time*Sorting.Time, Delivery.Time)
cor(Sorting.Time*Sorting.Time, Delivery.Time)

plot(Sorting.Time*Sorting.Time, log(Delivery.Time))
cor(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time*Sorting.Time, log(Delivery.Time))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree_stdt <- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time*Sorting.Time))

summary(reg2degree_stdt)

logpol_stdt <- predict(reg2degree_stdt)
expy_stdt <- exp(logpol_stdt)
err= st_dt$Delivery.Time-expy_stdt
err

sqrt(sum(err^2)/nrow(st_dt))  #RMSE

confint(reg2degree_stdt,level=0.95)
predict(reg2degree_stdt,interval="confidence")

reg2degree_stdt$fitted.values

# visualization
library(ggplot2)
ggplot(data = st_dt, aes(x = Sorting.Time + I(Sorting.Time^2), y = log(Delivery.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = st_dt, aes(x=Sorting.Time+I(Sorting.Time^2), y=logpol_stdt))

cor(reg2degree_stdt$fitted.values,Delivery.Time)
