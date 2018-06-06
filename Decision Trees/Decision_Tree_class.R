install.packages("rpart.plot")
library(rpart)
library(rpart.plot)



training_data <- read.csv(file = "Final_Training_Data.csv")
test_data <-read.csv(file = "Final_Test_Data.csv")
tree <- rpart(response~., training_data, method="class")


#plots the tree
prp(tree)

#Plots the cross validation results
plotcp(tree)

#Plots relative error and the number of splits.
rsq.rpart(tree)

#Pruning the tree based on the complexity parameter calculated by the plotcp()
#Select the least cp value for which the relative error is lesser and a more accurate split is created
prune_tree<- prune(tree, cp=0.011)

#Plotting the pruned tree 
prp(prune_tree)

#Predicting the response on the test data 
predict_data <- predict(prune_tree, test_data, type = "class")

#Creating a column in test_data for the predicted_data 
test_data$response_predicted <- predict_data

#Calculating the accuracy of the predicted response against the original response
count = 0
for(i in 1:nrow(test_data)){
  if(test_data[i,53] == test_data[i,54]){
    count = count + 1
  }
}
count/9382*100
