#svm
library(rpart)
library(rpart.plot)
library(kernlab)
library(ggplot2)
library(caret)
library(dplyr)
library(e1071)

View(dfFactor)
str(dfFactor)

View(df_clean)


#
cartTree <- rpart(PromotionStance ~ ., data = dfFactor)
cartTree
prp(cartTree, faclen = 0, cex = 0.8, extra = 1)

dfFactor_revised<-dfFactor[,-8:-9]
svmFrame<-df_clean[df_clean$PromotionStance=="Detractor" | df_clean$PromotionStance=="Promoter",]
svmFrame$Gender<-as.factor(svmFrame$Gender)
svmFrame$Type.of.Travel<-as.factor(svmFrame$Type.of.Travel)
svmFrame$Class<-as.factor(svmFrame$Class)
svmFrame$Partner.Name<-as.factor(svmFrame$Partner.Name)

svmFrame<-svmFrame[,c(-1,-2,-3)]
svmFrame<-svmFrame[,c(-13,-14,-16,-17,-21,-29,-30,-31)]
svmFrame<-svmFrame[,-15:-17]
svmFrame<-svmFrame[,-16:-20]

str(svmFrame)

svmFrame$PromotionStance <- as.factor(as.character(svmFrame$PromotionStance))

#check the variable we need to predict
table(svmFrame$PromotionStance)

#Drawing sample, according to 70% and 30%, so now we have two seperate sets
ind<-sample(1:2,nrow(svmFrame), replace = TRUE, prob = c(0.7,0.3))

#train set is data set 1, which occupies 70%
train_data<-svmFrame[ind==1,]

#test data is data set2, which occupies 30%
test_data<-svmFrame[ind==2,]

#take a look at train data and test data
str(train_data)
str(test_data)

#using all other variables to predict Promotin stance 
model1=svm(PromotionStance ~., data=svmFrame)
summary(model1)

svmOutput<-ksvm(PromotionStance~.,data=train_data, kernel="rbfdot", kpar="automatic",C=50,cross=3,prob.model=TRUE  )

svmOutput<-ksvm(PromotionStance~Age+Gender+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Partner.Name+Scheduled.Departure.Hour+Flight.Distance,data=train_data, kernel="rbfdot", kpar="automatic",C=50,cross=3,prob.model=TRUE  )
svmOutput

svmOutput<-ksvm(PromotionStance~Age+Gender+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Partner.Name,data=train_data, kernel="rbfdot", kpar="automatic",C=50,cross=3,prob.model=TRUE  )
svmOutput

#predict test data using predict function
svmPred<-predict(svmOutput,test_data)

#take a look at svmPred
head(svmPred)

# calculate models accuracy
compTable <- data.frame(svmPred, test_data$PromotionStance)
confMatrix <- table(compTable)
confMatrix
accuracy <- 1 - (sum(confMatrix) - sum(diag(confMatrix))) / sum(confMatrix)
accuracy




