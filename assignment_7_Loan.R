setwd("C://NU//ADS")
# install.packages("xlsx")
# install.packages("xlsxjars")
# install.packages("rJava")
# install.packages("neuralnet")
# install.packages("ISLR")

library(rJava)
library(xlsxjars)
library(xlsx)
library(neuralnet)
library(ISLR)

#Reading the loan file
loan<-read.xlsx(file="loan.xlsx", sheetIndex = 1)

#Converting the required varible values to numeric
res_status <- as.numeric(loan$Res_status)
occupation <- as.numeric(loan$Occupation)
job_status <- as.numeric(loan$Job_status)
liab_ref <- as.numeric(loan$Liab_ref)
acc_ref <- as.numeric(loan$Acc_ref)
decision <- as.numeric(loan$Decision)

#Saving the converted variables into a dataframe
dataFile <- data.frame(res_status,occupation,job_status,liab_ref,acc_ref,decision)
dataFile
head(dataFile)


#Dividing the data into Train and Test data
x<-sample(1:nrow(dataFile), nrow(dataFile)*0.80)
train_dataset <- dataFile[x,]
test_dataset <- dataFile[-x,]



#Applying neural network. Fitting the model.
nn <- neuralnet(train_dataset$decision ~ train_dataset$res_status+train_dataset$occupation+train_dataset$job_status+train_dataset$liab_ref+train_dataset$acc_ref,data=train_dataset, hidden=c(2,2,2),linear.output=FALSE)
predicted_neural_values <- compute(nn,test_dataset[1:5])
head(predicted_neural_values)
predicted_neural_values$net.result
table(test_dataset$decision, predicted_neural_values$net.result)

#plotting the neural network graph.
plot(nn)
