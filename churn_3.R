library(stats)
library(caTools)
library(Amelia)
library(dplyr)
library(randomForest)
library(DMwR)
library(dismo)
library(gbm)

accuracy <- vector(mode="numeric", length=10)
df_total = data.frame()
df_total2 = data.frame()
test_total=data.frame()
test_total2=data.frame()
for(i in 1:10){
  p <- paste("test.cluster",i,sep="")
  p_f <- paste(p,".csv",sep = "")
telecomDataframe1 <- read.csv(p_f)
telecomDataframe<- telecomDataframe1[,6:25]
telecomDataframe1<- telecomDataframe1[,c(2,6:25)]
telecomDataframe <- telecomDataframe[,-5]


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

sample <- sample.split(telecomDataframe1$Churn,SplitRatio=0.70)
trainData.temp <- subset(telecomDataframe1,sample==TRUE)
testData.temp <- subset(telecomDataframe1,sample==FALSE)
trainData <- trainData.temp[,-1]
testData <- testData.temp[,-1]
test_data <- data.frame(testData$Churn,testData.temp$name)
test_total <- rbind(test_total,test_data)

if((i==2)||(i==5)||(i==6)||(i==7)||(i==8)||(i==9)||(i==10)) {
  
telecomModel <-  randomForest(Churn ~ ., trainData,ntree=500)
test.predictions <- predict(telecomModel,newdata=testData,type="response")
#print(test.predictions)

fitted.results <- ifelse(test.predictions == "Yes",1,0)
df <- data.frame(fitted.results,testData.temp$name)
df_total <- rbind(df_total,df)

testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"

misClasificationError <- mean(fitted.results!=testData$Churn)
#print(misClasificationError)

accuracyRate <- 1-misClasificationError
accuracy[i] = accuracyRate
}
else if((i==1)||(i==3)||(i==4)){
  trainData1 <- trainData
  trainData1$Churn <- ifelse(trainData$Churn=="Yes",1,0)
  telecomModel1<-gbm.step(data=trainData1, gbm.x = 1:19, gbm.y = 20, family = "bernoulli")

  test.predictions <- predict(telecomModel1,newdata=testData,n.trees=800,type="response")

  fitted.results <- ifelse(test.predictions >0.5,1,0)
  df2 <- data.frame(fitted.results,testData.temp$name)
  df_total2 <- rbind(df_total2,df2)
  testData$Churn <- as.character(testData$Churn)
  testData$Churn[testData$Churn=="No"] <- "0"
  testData$Churn[testData$Churn=="Yes"] <- "1"

  misClasificationError <- mean(fitted.results!=testData$Churn)
  print(misClasificationError)

  accuracyRate <- 1-misClasificationError
  accuracy[i] <- accuracyRate
  
  
}
}


test_total$testData.Churn <- ifelse(test_total$testData.Churn == "Yes",1,0)

predicted.churn=rbind(df_total,df_total2)
predicted.actual <- merge(test_total, predicted.churn, by.x = "testData.temp.name", by.y = "testData.temp.name")

write.csv(predicted.churn,"predictions.csv")
write.csv(test_total,"actual.csv")
write.csv(predicted.actual,"predict_actual.csv")

missclassify <- mean(predicted.actual$fitted.results!=predicted.actual$testData.Churn)
print(missclassify)

accuracyi <- 1-missclassify
print(accuracyi)

slices <- c(missclassify,accuracyi)
lbls <- c("MissClassified","Accurate")
pie3D(slices, labels = lbls, main="Accuracy pie chart",col=rainbow(length(lbls)))
library(plotly)
v <- data.frame(missclassify,accuracyi)
lbls = c("Correctly.Classified","Miss.Classified")
pct <- c(missclassify,accuracyi)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels  
plot_ly(v,labels = lbls,values = slices, type = 'pie') %>%
  layout(title = 'Accuracy pie chart',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 





