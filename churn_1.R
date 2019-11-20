library(stats)
library(caTools)
library(Amelia)
library(randomForest)
library(caret)
library(DMwR)
telecomDataframe <- read.csv(file="E:\\TelcomChurn.csv")

any(is.na(telecomDataframe))

library(imputeR)
telecomDataframe <- knnImputation(telecomDataframe,k=10,scale= T,meth="mean")

sapply(telecomDataframe, function(x) sum(is.na(x)))

#remove rows with all NAN
telecomDataframe<-telecomDataframe[complete.cases(telecomDataframe),]


missmap(telecomDataframe,col=c("yellow","red"))

group_tenure <- function(tenure){
  if (tenure >= 0 && tenure <= 6){
    return('0-6 Month')
  }else if(tenure > 6 && tenure <= 12){
    return('6-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <=36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <=48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 62){
    return('48-62 Month')
  }else if (tenure > 62){
    return('> 62 Month')
  }
}


telecomDataframe$tenure_interval <- sapply(telecomDataframe$tenure,group_tenure)
telecomDataframe$tenure_interval <- as.factor(telecomDataframe$tenure_interval)
str(telecomDataframe)

telecomDataframe <- select(telecomDataframe,-customerID,-tenure)


telecomDataframe$MultipleLines <- as.character(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.character(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.character(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.character(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.character(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.character(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.character(telecomDataframe$StreamingMovies)


telecomDataframe$MultipleLines[telecomDataframe$MultipleLines=="No phone service"] <- "No"
telecomDataframe$OnlineSecurity[telecomDataframe$OnlineSecurity=="No internet service"] <- "No"
telecomDataframe$OnlineBackup[telecomDataframe$OnlineBackup=="No internet service"] <- "No"
telecomDataframe$DeviceProtection[telecomDataframe$DeviceProtection=="No internet service"] <- "No"
telecomDataframe$TechSupport[telecomDataframe$TechSupport=="No internet service"] <- "No"
telecomDataframe$StreamingTV[telecomDataframe$StreamingTV=="No internet service"] <- "No"
telecomDataframe$StreamingMovies[telecomDataframe$StreamingMovies=="No internet service"] <- "No"

telecomDataframe$MultipleLines <- as.factor(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.factor(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.factor(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.factor(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.factor(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.factor(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.factor(telecomDataframe$StreamingMovies)


set.seed(123)

sample <- sample.split(telecomDataframe$Churn,SplitRatio=0.70)
trainData <- subset(telecomDataframe,sample==TRUE)
testData <- subset(telecomDataframe,sample==FALSE)
trainData.model=trainData[,-1]

#fitting the random forest model on the train dataset of the whole data 
fit <- randomForest(Churn ~ ., trainData[,-1],ntree=800)
test.predictions= predict(fit,testData)
mat<- confusionMatrix(test.predictions,testData$Churn)
mat
fitted.results <- ifelse(test.predictions == "Yes",1,0)
testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"
misClasificationError <- mean(fitted.results!=testData$Churn)
print(misClasificationError)

accuracyRate <- 1-misClasificationError
print(accuracyRate)

results <- cbind(fitted.results,testData$Churn)
colnames(results) <- c("predicted","actual")
results <- as.data.frame(results)
#print(results)
