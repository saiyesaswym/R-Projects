
str(quality)
#Poor care is the target variable
#0-good care 1-poor care

table(quality$PoorCare)
#98 received goodcare and 33 received poorcare

base <- 98/131
#Base line value can be calucated by taking the percentage of good care

set.seed(103)
spl = sample(nrow(quality),nrow(quality)*0.75)

train = quality[spl,]
test = quality[-spl,]

model3 <- glm(PoorCare ~ OfficeVisits + Narcotics,data = train,family = binomial)

summary(model3)             

predicttrain <- predict(model3,type = "response")
summary(predicttrain)

tapply(predicttrain,train$PoorCare,mean)

table(train$PoorCare,predicttrain>0.5)
#THis gives the confusion matrix or classification matrix
#for a threshold of 0.5

sensitiviy <- 10/(10+15)

specitivity <- 71/(71+2)

ROCRPred <- prediction(predicttrain,train$PoorCare)

ROCRPerf <- performance(ROCRPred,"tpr","fpr")

plot(ROCRPerf,colorize=TRUE)

plot(ROCRPerf,colorize=TRUE,print.cutoffs.at=c(0,1,0.1),text.adj=c(-0.2,1.7))
#Adding threshold labels to the plot
