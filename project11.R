rm(list=ls())                
setwd("C:/Users/Acer/Desktop/coursera")
library(caret)
library(randomForest)
library(rpart)
url.train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url.test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(url.train), na.strings = c("NA", "", "#DIV0!"))
testing <- read.csv(url(url.test), na.strings = c("NA", "", "#DIV0!"))
sameColumsName <- colnames(training) == colnames(testing)
colnames(training)[sameColumsName==FALSE]
training<-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]
head(colnames(training), 10)
training <- training[,8:dim(training)[2]]
testing <- testing[,8:dim(testing)[2]]
set.seed(12345)
inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
trainingCV <- training[inTrain,]
testingCV <- training[-inTrain,]
dim(trainingCV)
dim(testingCV)
qplot(accel_arm_x, accel_arm_y, col=classe, data=trainingCV)
qplot(accel_forearm_x, accel_forearm_y, col=classe, data=trainingCV)
modelCTree <- rpart(classe ~ ., data=trainingCV, method="class")
predictionCTree <- predict(modelCTree, testingCV, type="class")
CTree <- confusionMatrix(predictionCTree, testingCV$classe)
CTree
library(rpart.plot)
rpart.plot(modelCTree)
modelRF <- randomForest(classe ~ ., data=trainingCV, method="class")
predictionRF <- predict(modelRF, testingCV, type="class")
RF <- confusionMatrix(predictionRF, testingCV$classe)
RF
CV <- testingCV
CV$GOODpred <- testingCV$classe == predictionRF
qplot(accel_forearm_x, accel_forearm_y, col=GOODpred, data=CV)
FinalPrediction <- predict(modelRF, testing)
kable(t(data.frame(FinalPrediction)))
