
data_hw2 <- read.csv("dataMain.csv")

summaryfile <- summary(data_hw2)

summaryfile

hist(data_hw2$clopidogrel,main = "Histogram of Clopidogrel")
hist(data_hw2$ASPIRIN.USE,main = "Histogram of Aspirin")
hist(data_hw2$Height,main = "Histogram of Height")
hist(data_hw2$Weight,main = "Histogram of weight")
hist(data_hw2$BSA,main = "Histogram of Body Surface Area")
hist(data_hw2$diabetes,main = "Histogram of Diabetes")
hist(data_hw2$Mortality.score,main = "Histogram of Mortality score")
hist(data_hw2$X24EBL,main = "Histogram of Blood Loss")

standard_deviation<-apply(data_hw2,2,sd)

Interquartile_range <-apply(data_hw2,2,IQR)


qqnorm(data_hw2$Height,main = "Q-Q plot of Height")
qqline(data_hw2$Height)
shapiro.test(data_hw2$Height)

log_height <- log(data_hw2$Height)
qqnorm(log_height,main = "Q-Q plot of Log Height")
shapiro.test(log_height)

qqnorm(data_hw2$Weight,main = "Q-Q plot of Weight")
qqline(data_hw2$Weight)
shapiro.test(data_hw2$Weight)

qqnorm(data_hw2$BSA,main = "Q-Q plot of Body Surface Area")
qqline(data_hw2$BSA)
shapiro.test(data_hw2$BSA)

qqnorm(data_hw2$Mortality.score,main = "Q-Q plot of Mortality Score")
qqline(data_hw2$Mortality.score)
shapiro.test(data_hw2$Mortality.score)

log_mortality <- log(data_hw2$Mortality.score)
qqnorm(log_mortality,main = "Q-Q plot of Log Mortality Score")
shapiro.test(log_mortality)

qqnorm(data_hw2$AGE,main = "Q-Q plot of Age")
qqline(data_hw2$AGE)
shapiro.test(data_hw2$AGE)

qqnorm(data_hw2$X24EBL,main="Q-Q plot of Blood loss parameter")
qqline(data_hw2$X24EBL)
shapiro.test(data_hw2$X24EBL)


