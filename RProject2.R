#Finding Correlation 

cor(data_hw2)

#Finding covariance matrix
cov(data_hw2)

cor(data_hw2$Height,data_hw2$Weight)


cor(data_hw2,data_hw2$X24EBL)
cor(data_hw2,data_hw2$log24EBL)


plot(data_hw2$Height,data_hw2$Weight,main = "Plot of Height vs Weight")

plot(data_hw2$BSA,data_hw2$Weight,main="Plot of Body SurfaceArea Vs Weight")

plot(data_hw2$Mortality.score,data_hw2$X24EBL,main="Plot of Mortality Score vs Blood Loss")

#PRINCIPAL COMPONENT ANALYSIS
pca_data <- data_hw2[,c("AGE","Weight","Height")]

pca <- prcomp(pca_data)
pca

plot(pca,type="l",main = "PCA PLOT")

summary(pca)

biplot(pca,scale=0)

#SINGLE VALUE DECOMPOSITION

sv <- svd(pca_data)
sv

plot(sv$u[,1],xlab = "Row",ylab = "First left singular vector",pch=15)


plot(sv$u[,2],xlab = "Row",ylab = "First left singular vector",pch=15)

#VarianceExplained
plot(sv$d^2/sum(sv$d^2), xlab="Column", ylab = "Singular value", pch=19)

#Relation bn SVD and PCA
plot(pca$rotation[ ,1], sv$v[ ,1], pch= 19, xlab ="Principal Component 1", ylab=
       "Right Singular Vector 1", type='b')

#NonParametric test
wilcox.test(data_hw2$X24EBL~data_hw2$clopidogrel)


#Eliminating BSA variable
new_data <- subset(data_hw2,select = -c(BSA))
new_data

cor(new_data$clopidogrel,new_data$log24EBL)

data_lm <- lm(log24EBL~Height+Weight+AGE+clopidogrel,data = new_data)
summary(data_lm)

