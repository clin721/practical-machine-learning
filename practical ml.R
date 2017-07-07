library(caret)
library(gbm)
library(randomForest)
library(knitr)



training = read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing = read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

training = training[, colSums(is.na(training)) <=.4*dim(training)[1]]
testing = testing[, colSums(is.na(testing)) <=.4*dim(testing)[1]]

training = training[, -c(1:7)]
testing = testing[, -c(1:7)]

set.seed(1002)
inTrain = createDataPartition(training$classe, p=0.6, list=FALSE)
trainset = training[inTrain, ]
testset = training[-inTrain, ]

rfmodel = train(classe ~ ., data = trainset, method = "rf", 
                trControl = trainControl(method = "cv", number = 5))

print(rfmodel, digits = 3)

rfPredict = predict(rfmodel, testset)
rfConf = confusionMatrix(testset$classe, rfPredict)
rfConf$table
rfConf$overall[1]

gbmodel = train(classe ~ ., data = trainset, method = "gbm", verbose = FALSE,
                trControl = trainControl(method = "cv", number = 5))

print(gbmodel, digits = 3)

gbPredict = predict(gbmodel, testset)
gbConf = confusionMatrix(testset$classe, gbPredict)
gbConf$table
gbConf$overall[1]


predict(rfmodel, testing)
predict(gbmodel, testing)








