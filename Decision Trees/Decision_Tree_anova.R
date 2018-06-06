install.packages("rpart.plot")
install.packages("rpart")
library(rpart)
library(rpart.plot)

setwd("C:/NU/ADS")


training_data <- read.csv(file = "Final_Training_Data.csv")
test_data <-read.csv(file = "Final_Test_Data.csv")
tree <- rpart(response~., training_data, method = "anova")
summary(tree)
#plots the tree
prp(tree)

printcp(tree)

#Plots the cross validation results
plotcp(tree)

#Plots relative error and the number of splits.
rsq.rpart(tree)

#Pruning the tree based on the complexity parameter calculated by the plotcp()
#Select the least cp value for which the relative error is lesser and a more accurate split is created
prune_tree<- prune(tree, cp=0.013)

#Plotting the pruned tree 
prp(prune_tree)

summary(prune_tree)
#Predicting the response on the test data 
predict_data <- round(predict(prune_tree, test_data, type = "vector"))

#Creating a column in test_data for the predicted_data 
test_data$response_predicted <- predict_data

summary(test_data)
#Root Mean Square Error
RMSE <- sqrt(mean((test_data[,53]- test_data[,54])^2))
RMSE
#2.183541

#Calculating the accuracy of the predicted response against the original response
count = 0
for(i in 1:nrow(test_data)){
  if(test_data[i,53] == test_data[i,54]){
    count = count + 1
  }
}
count/9382*100

