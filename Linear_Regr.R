library(ggplot2)

str(diamonds)

#price - target variable

hist(diamonds$price)
summary(diamonds$price)
#Not following a normal distribution

summary(log(diamonds$price))
diamonds$log_price <- log(diamonds$price)
diamonds$price <- NULL
#Log transformation is performed

cor(diamonds$carat,diamonds$log_price)
#Correlation between input and target variable

set.seed(101)
idx = sample(nrow(diamonds),nrow(diamonds)*0.8)

train = diamonds[idx,]
test = diamonds[-idx,]
#Dividing the train and test set

model1 = lm(log_price~.,data=train)
#Creating the model

model2 = lm(log_price~carat+cut+color,data = train)

summary(model1)

names(model1)

model1$residuals
#Gives the error terms or the residuals for model 1

SSE <- sum(model1$residuals^2)
#This is the sum of squared errors for model1 

plot(model1$fitted.values,model1$residuals)

test$y_pred = predict(model1,newdata = test)
#predicting the values for test set

test$diff = test$log_price-test$y_pred

cor(test$y_pred,test$diff)

test$diff_sq = test$diff**2

rmse = sqrt(mean(test$diff_sq))

error_percent = rmse/mean(test$log_price)
error_percent

cd <- cooks.distance(model1)
