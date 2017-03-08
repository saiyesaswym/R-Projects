tdata <- read.csv("titaniclist.csv")

str(tdata)

table(tdata$survived)

sum(is.na(tdata))
colSums(is.na(tdata))

set.seed(106)
sam1 = sample(nrow(tdata),nrow(tdata)*0.75)

train = tdata[sam1,]
test = tdata[-sam1,]

tdata$ticket <- as.character(tdata$ticket)

tdata$cabin <- as.character(tdata$cabin)

tdata$survived <- as.factor(tdata$survived)

model1 <- glm(survived ~ pclass+sex+age+sibsp,data = train,family = binomial)

summary(model1)

test$pred <- predict(model1,newdata = test,type = "response")

test$pred_class <- ifelse(test$pred>=0.5,1,0)

table(test$survived,test$pred_class)

accuracy <- (140+60)/(140+60+17+27)

precision <- 60/(60+17)

recall <- 60/(60+27)

#-----------------------------
require(randomForest)

model_rf <- randomForest(survived~pclass+sex+age+sibsp+parch+fare+embarked,data = train,na.action = na.omit,ntree=100)

varImpPlot(model_rf)

test$predrf <- predict(model_rf,newdata = test)

table(test$survived,test$predrf)
#------------------------------------------


my_tree_two <- rpart(survived ~ pclass + sex + age + sibSp + parch + fare + embarked, data = train, method = "class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)
