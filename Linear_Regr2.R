str(Wage)

#wage - target variable

summary(log(Wage$wage))
hist(log(Wage$wage))

#Log transformation is following normal distribution

Wage$wage <- NULL

cor(Wage$year,Wage$logwage)
#No proper correlation between input and target

set.seed(102)
ind <- sample(nrow(Wage),nrow(Wage)*0.8)

train <- Wage[ind,]
test <- Wage[-ind,]

#Dividing the data set into train and test sets

model2 = lm(Wage$logwage~Wage$year+Wage$age,data=train)

summary(model2)

test$wage_pred <- predict(model2,newdata = test)
