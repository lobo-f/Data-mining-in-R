setwd("C://NU//ADS")
# install.packages("xlsx")
# install.packages("xlsxjars")
# install.packages("rJava")
# install.packages("neuralnet")
# install.packages("ISLR")
#install.packages("ade4")

library(rJava)
library(xlsxjars)
library(xlsx)
library(neuralnet)
library(ISLR)
library(ade4)

#Previously cleaned data derived from the project
train_cleaned<-read.csv(file="Final_Training_Data.csv")

#Converting the responsible variable from continuous to categorical variables. A total of 8 variables are derived. 
train_response <- acm.disjonctif(train_cleaned[c("response")])

#Deleting the initial response variable from the train data
train_cleaned$response = NULL

#Adding the categorical response variables to the previous train data.
train_insurance <- data.frame(c(train_cleaned,train_response))

#Selecting only the main variables from the data based on the decision tree derived from the project
dataFile <- data.frame(train_insurance$BMI,train_insurance$Medical_History_4.1, train_insurance$Medical_History_23.1,train_insurance$Medical_History_27.1,train_insurance$Medical_History_29.1,train_insurance$Medical_History_22.1, train_insurance$Medical_Keyword_3,train_insurance$response.1,train_insurance$response.2,train_insurance$response.3,train_insurance$response.4,train_insurance$response.5,train_insurance$response.6,train_insurance$response.7, train_insurance$response.8)

#Test data got from the midterm project 
test_insurance<-read.csv(file="Final_Test_Data.csv")

#Selecting only the first 700 rows to decrease the running time
train_insurance <- train_insurance[1:700,]

#Selecting only the main variables to match with the train data
test_dataFile <- data.frame(test_insurance$BMI,test_insurance$Medical_History_4.1, test_insurance$Medical_History_23.1,test_insurance$Medical_History_27.1,test_insurance$Medical_History_29.1,test_insurance$Medical_History_22.1, test_insurance$Medical_Keyword_3, test_insurance$response)


#applying neural network to train data. Fitting the model
nn <- neuralnet(train_insurance$response.1 + train_insurance$response.2 + train_insurance$response.3 + train_insurance$response.4 + train_insurance$response.5 +train_insurance$response.6 +train_insurance$response.7 + train_insurance$response.8 ~ train_insurance$BMI+train_insurance$Medical_History_4.1+ train_insurance$Medical_History_23.1+train_insurance$Medical_History_27.1+train_insurance$Medical_History_29.1+train_insurance$Medical_History_22.1+ train_insurance$Medical_Keyword_3, data=dataFile, hidden=c(3),linear.output=FALSE,stepmax = 1e6)


#Using sapply to round the values
predicted_neural_values$net.result <- sapply(predicted_neural_values$net.result,round,digits=0)

#Plotting the neuralnetwork graph
plot(nn)


#Predicting
predicted_neural_values <- compute(nn,test_dataFile[1:7])
head(predicted_neural_values)
predicted_neural_values$net.result
idx <- apply(predicted_neural_values$net.result , 1, which.max)
pred <- c("1","2","3","4","5","6","7","8")[idx]
table(test_dataFile$Response,pred)
pred

